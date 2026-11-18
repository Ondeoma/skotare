-- skotare のスキーマ（適用済みのファイルは書き換えず、変更は新しい番号の V<n>__*.sql を追加する）

create table app_user (
  id             uuid primary key,
  sub            text not null unique,           -- OIDC の sub（開発用ログインは dev:名前）
  name           text not null,
  email          text,
  avatar_url     text,                           -- IdP の画像
  avatar_version int,                            -- 保存したアイコンの版（null なら IdP の画像）
  timezone       text not null default 'Asia/Tokyo',
  disabled       boolean not null default false,
  created_at     timestamptz not null default now(),
  last_login_at  timestamptz
);
create index app_user_email_idx on app_user (lower(email));

-- ユーザーが保存したアイコン（画面で 256×256 にした画像）. あれば IdP の avatar_url より優先する
create table user_avatar (
  user_id      uuid primary key references app_user(id) on delete cascade,
  content_type text not null check (content_type in ('image/png', 'image/jpeg', 'image/gif', 'image/webp')),
  data         bytea not null,
  updated_at   timestamptz not null default now()
);

create table session (
  token_hash bytea primary key,                  -- Cookie の値の SHA-256（値そのものは保存しない）
  user_id    uuid not null references app_user(id) on delete cascade,
  id_token   text,                               -- IdP からログアウトするときの id_token_hint
  created_at timestamptz not null default now(),
  expires_at timestamptz not null
);
create index session_expires_idx on session (expires_at);

-- OIDC のログイン途中の状態（複数ノードのどこに戻ってきてもよいよう DB に置く）
create table login_state (
  state      text primary key,
  verifier   text not null,
  nonce      text not null,
  return_to  text not null default '/',
  created_at timestamptz not null default now()
);

-- API トークン（REST API と MCP を Bearer で呼ぶため）. 値は SHA-256 だけを保存する
create table api_token (
  id           uuid primary key,
  user_id      uuid not null references app_user(id) on delete cascade,
  name         text not null,
  prefix       text not null,                    -- 画面に出す先頭部分（skotare_xxxxxx）
  token_hash   bytea not null unique,
  scope        text not null check (scope in ('read', 'write')),
  created_at   timestamptz not null default now(),
  expires_at   timestamptz,
  last_used_at timestamptz,
  revoked_at   timestamptz
);
create index api_token_user_idx on api_token (user_id);

-- プッシュ通知（PWA）. VAPID の鍵（全ノードで同じものを使う）と、ブラウザ・端末ごとの購読
create table push_config (
  id            int primary key check (id = 1),
  vapid_public  text not null,
  vapid_private text not null,
  created_at    timestamptz not null default now()
);

create table push_subscription (
  id           uuid primary key,
  user_id      uuid not null references app_user(id) on delete cascade,
  endpoint     text not null unique,
  p256dh       text not null,
  auth         text not null,
  user_agent   text,
  created_at   timestamptz not null default now(),
  last_used_at timestamptz
);
create index push_subscription_user_idx on push_subscription (user_id);

-- ---------------------------------------------------------------- 監視

-- 監視の対象. kind ごとの設定は config（JSON. 形は shared の MonitorConfig）.
-- 監視は拠点ごとに確かめ（monitor_region）、健全な拠点の票を合わせて status を決める
create table monitor (
  id             uuid primary key,
  name           text not null,
  kind           text not null check (kind in ('http', 'tcp', 'dns', 'ping')),
  config         jsonb not null,
  interval_sec   int not null check (interval_sec between 20 and 86400),
  timeout_ms     int not null check (timeout_ms between 500 and 60000),
  retries        int not null default 1 check (retries between 0 and 10), -- 停止と判断するまでに、続けて失敗してよい回数（拠点ごとに数える）
  enabled        boolean not null default true,
  description    text not null default '',
  -- canary: 基準点（拠点そのものが外へ届くかを確かめるための監視. 拠点の半分を超える基準点に届かなければ、その拠点を疑う）
  canary         boolean not null default false,
  -- 停止と判断するのに要る、停止と見ている拠点の数（0 は過半数. 健全な拠点の数より多ければ、すべての拠点）
  confirm_regions int not null default 0 check (confirm_regions between 0 and 20),
  -- 状態: pending（まだ確かめていない）/ up / down / paused
  status         text not null default 'pending' check (status in ('pending', 'up', 'down', 'paused')),
  status_since   timestamptz not null default now(),
  last_checked_at timestamptz,                    -- 一覧に出す最後の結果（判定に数える拠点のもの. 停止の間は原因）
  last_latency_ms int,
  last_message   text,
  created_by     uuid references app_user(id) on delete set null,
  created_at     timestamptz not null default now(),
  updated_at     timestamptz not null default now()
);

-- ---------------------------------------------------------------- 拠点（REGION）
--  - 監視は拠点ごとに確かめる（同じ拠点のノードどうしは monitor_region の行を「for update skip locked」で取り合う）
--  - 拠点ごとの状態（monitor_region.status）を、健全な拠点の票で合わせて、監視の状態（monitor.status）を決める
--  - 拠点そのものの健全性（region.health）を、心拍・基準点・ほかの拠点との食い違いで判定し、疑わしい拠点の票は数えない
--    （「skotare 側（その拠点）の故障」と「監視先の故障」を分ける）

-- 拠点. ノードが心拍で作る. health: ok（健全）/ suspect（skotare 側の異常の疑い）/ offline（動いているノードがない）
create table region (
  name       text primary key,
  health     text not null default 'ok' check (health in ('ok', 'suspect', 'offline')),
  reason     text not null default '',
  since      timestamptz not null default now(),
  created_at timestamptz not null default now()
);

-- 拠点ごとの確認の状態（停止の判断までの失敗の回数・次の確認の時刻も拠点ごと）
create table monitor_region (
  monitor_id      uuid not null references monitor(id) on delete cascade,
  region          text not null references region(name) on delete cascade,
  status          text not null default 'pending' check (status in ('pending', 'up', 'down')),
  status_since    timestamptz not null default now(),
  fail_streak     int not null default 0,          -- 続けて失敗した回数
  last_checked_at timestamptz,
  last_latency_ms int,
  last_message    text,
  next_check_at   timestamptz not null default now(),
  primary key (monitor_id, region)
);
create index monitor_region_due_idx on monitor_region (region, next_check_at);

-- 確認の結果（既定で30日残す）. node・region はどのノード・拠点が確かめたか.
-- up はそのときの監視の総合の判定（判定に数える拠点の結果だけ. 24時間の稼働率と一覧の帯はこれで数える. 未確認・保留は null）
create table check_result (
  monitor_id  uuid not null references monitor(id) on delete cascade,
  checked_at  timestamptz not null,
  ok          boolean not null,
  latency_ms  int,
  message     text not null default '',
  node        text not null default '',
  region      text not null default 'default',
  up          boolean
);
create index check_result_monitor_idx on check_result (monitor_id, checked_at desc);
create index check_result_at_idx on check_result (checked_at);
-- 一覧の24時間の稼働率と最近の帯（判定のある結果だけを、表を読まずに数える）
create index check_result_up_idx on check_result (monitor_id, checked_at) include (up) where up is not null;

-- 日ごとの稼働率の集計（確認の結果は30日で消すが、ステータスページの90日の帯と30日の稼働率はこの表から読む）.
-- 確認のたびに、そのときの監視の判定（check_result.up）を足し込む. 日付は UTC
create table check_daily (
  monitor_id uuid not null references monitor(id) on delete cascade,
  day        date not null,
  up_n       int not null default 0,
  total_n    int not null default 0,
  primary key (monitor_id, day)
);

-- 障害（停止してから復旧するまで）. 復旧していないものは resolved_at が null
create table incident (
  id          uuid primary key,
  monitor_id  uuid not null references monitor(id) on delete cascade,
  started_at  timestamptz not null,
  resolved_at timestamptz,
  cause       text not null default ''
);
create index incident_monitor_idx on incident (monitor_id, started_at desc);
create unique index incident_open_idx on incident (monitor_id) where resolved_at is null;

-- 通知先（stui の受信 Webhook・skereru のメール・汎用の Webhook）. 秘密の値（API キー・URL）は APP_SECRET の鍵で暗号化して secret に入れる
create table notifier (
  id           uuid primary key,
  name         text not null,
  kind         text not null check (kind in ('stui', 'skereru', 'webhook')),
  config       jsonb not null,                    -- 秘密でない設定（skereru の差出人・宛先など）
  secret       text not null default '',          -- 暗号化した秘密の値
  all_monitors boolean not null default true,     -- すべての監視の通知を送る（false なら notifier_monitor で選んだものだけ）
  enabled      boolean not null default true,
  created_at   timestamptz not null default now()
);

create table notifier_monitor (
  notifier_id uuid not null references notifier(id) on delete cascade,
  monitor_id  uuid not null references monitor(id) on delete cascade,
  primary key (notifier_id, monitor_id)
);

-- 送る通知の行列（状態が変わったトランザクションで入れる. 送れなければ間隔を延ばして送り直す. 複数ノードで2度送らないよう skip locked で取る）
create table notification_outbox (
  id          uuid primary key,
  notifier_id uuid not null references notifier(id) on delete cascade,
  payload     jsonb not null,
  attempts    int not null default 0,
  next_at     timestamptz not null default now(),
  sent_at     timestamptz,
  last_error  text,
  created_at  timestamptz not null default now()
);
create index notification_outbox_due_idx on notification_outbox (next_at) where sent_at is null;

-- ステータスページ（ログインしない人にも /s/{slug} で見せる）
create table status_page (
  id          uuid primary key,
  slug        text not null unique check (slug ~ '^[a-z0-9][a-z0-9-]{1,40}$'),
  title       text not null,
  description text not null default '',
  notice      text not null default '',            -- お知らせ（メンテナンスの予定など）
  public      boolean not null default true,       -- false ならログインした人だけ
  created_at  timestamptz not null default now()
);

create table status_page_monitor (
  page_id      uuid not null references status_page(id) on delete cascade,
  monitor_id   uuid not null references monitor(id) on delete cascade,
  position     int not null default 0,
  display_name text,                               -- ページに出す名前（なければ監視の名前）
  primary key (page_id, monitor_id)
);

-- 動いているノード（冗長構成の様子を画面に出す. 心拍を30秒ごとに書く）. region は拠点の名前
create table node (
  name       text primary key,
  region     text not null default 'default',
  version    text not null,
  started_at timestamptz not null,
  seen_at    timestamptz not null
);
