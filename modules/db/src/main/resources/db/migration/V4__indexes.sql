-- 検索・掃除・削除（外部キーの cascade）で表を全部読まないための索引

-- 障害の一覧（監視を絞らずに新しい順に読む）
create index incident_started_idx on incident (started_at desc);

-- 通知先を消すとき・無効にした通知先の分を閉じるとき
create index notification_outbox_notifier_idx on notification_outbox (notifier_id);
-- 送り終えた通知の掃除（30日より前）
create index notification_outbox_sent_idx on notification_outbox (sent_at) where sent_at is not null;

-- 監視を消すとき（主キーは通知先・ページが先頭なので、監視からは引けない）
create index notifier_monitor_monitor_idx on notifier_monitor (monitor_id);
create index status_page_monitor_monitor_idx on status_page_monitor (monitor_id);

-- 失効させた API トークンの掃除
create index api_token_revoked_idx on api_token (revoked_at) where revoked_at is not null;
