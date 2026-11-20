# skotare

<p align="center"><img src="frontend/public/logo.svg" width="128" height="128" alt="skotare のロゴ"></p>

skotare は、Web サイト・サーバーを外から監視し、ゲスト（ログインしない人）にステータスページを見せるサービスです。
ログインは OpenID Connect（OIDC）。複数台・複数の拠点で動かせます。ライセンス: MIT

Scala 3（http4s・doobie・PostgreSQL）、画面は Scala.js + [Scalactive](https://github.com/Ondeoma/scalactive)、Vite。設計は [docs/design.md](docs/design.md) を参照してください。

- **監視の種類**
  - **HTTP(S)**: メソッド・ヘッダー・本文、タイムアウト、期待する応答コード（`200-299,301` のような範囲）、
    本文に含まれるべき文字・含まれてはいけない文字・正規表現、リダイレクトをたどるか、TLS の証明書の期限（残りの日数）
  - **TCP**: ホストとポートに接続できるか
  - **DNS**: A・AAAA・CNAME・MX・TXT・NS を引けるか、答えに期待する値があるか（問い合わせる DNS サーバーを選べる）
  - **PING**: ICMP の echo に応えるか（OS の `ping` コマンドを使う）
- **判定**: 確認の間隔（20秒〜1日）、続けて失敗してよい回数（再試行。超えたら停止と判断。判断する前の失敗では20秒後に確かめ直す）
- **画面**: 監視の一覧（状態・最近の30区間（確認の間隔ごと）の判定・24時間/30日の稼働率・応答時間）、詳細（応答時間のグラフ・90日の日ごとの稼働率・障害・最近100回の結果とノード）、
  「試す」（保存せずに今すぐ確かめる）、今すぐ確かめる、一時停止・再開、障害の一覧、ダークモード、スマートフォン
- **ステータスページ**（`/s/{名前}`）: ゲストに、名前・状態・90日の稼働率の帯・発生中と過去14日の障害・お知らせ（メンテナンスの予定など）を見せる。
  監視の URL・ホストは見せない。ページでの名前を付け替えられる。ログインした人だけに見せるページも作れる
- **通知**: 停止・復旧したとき、停止中に設定を変えて障害を閉じたとき、拠点の異常・回復のときに
  - **[stui](https://github.com/Ondeoma/stui)**（チャット）: stui と連携して、参加しているチャンネルを選んで投稿（または、チャンネルの受信 Webhook に投稿）
  - **[skereru](https://github.com/Ondeoma/skereru)**（メール）: skereru の送信 API（APIキー・send のスコープ）でメールを送る
  - **Webhook**: 任意の URL に JSON を POST（`event`・`status`・`monitor`・`message`・`at`・`downSince`・`url`）
  - **プッシュ通知**（PWA）: 受け取ると決めた端末に
  - 送れなければ間隔を延ばして送り直す（最大10回）。監視ごとに送る先を選べる。「試しに送る」で設定を確かめられる
- **冗長構成**: 同じ DB を使うノードを何台でも動かせる。確かめる時刻の来た監視は、同じ拠点のノードが DB で取り合う（`for update skip locked`）ので
  同じ監視を同じ拠点の2台で確かめず、台数を増やすと分担する。ノードが止まっても、同じ拠点のほかのノードが次の時刻に確かめる。
  通知は状態が変わったトランザクションで行列に入れ、1回だけ送る。拠点とノードの生存は画面（監視の一覧の上）とメトリクスで分かる
- **複数の拠点**（`REGION`）: 拠点ごとにすべての監視を確かめ、「skotare 側（拠点）の故障」と「監視先の故障」を分ける
  - 監視の状態は、健全な拠点の票で決める。既定は過半数の拠点が停止と見たら停止（監視ごとに「1つの拠点」「すべての拠点」なども選べる）
  - 拠点の健全性: 動いているノードがない → **停止**。基準点（canary に指定した監視）の過半数に届かない、または
    ほかの拠点では正常な監視先の半分以上（3件以上）にその拠点だけが届かない → **異常の疑い**。どちらもその拠点の結果は判定に数えず、
    拠点の異常として知らせる（監視先の停止とは知らせない）。健全な拠点がなければ、監視先の状態は変えずに保留する
  - 画面: 拠点ごとの健全性と理由、監視ごとに拠点ごとの状態（判定に数えているか）、一部の拠点からだけ届かない監視
- **アイコン**: 設定で画像を登録できる（正方形に切り取って縮める）。登録しない人は IdP のアイコンか名前の1文字目
- **API・AI**: REST API（API トークン. 参照のみ・読み書き）、MCP（`get_status`・`list_monitors`・`get_monitor`・`test_monitor`・`create_monitor`・
  `update_monitor`・`pause_monitor`・`resume_monitor`・`check_now`・`delete_monitor`・`list_incidents`・`list_status_pages`・`call_api` など）
- **観測**（[skorimako](https://github.com/Ondeoma/skorimako)）: ログ・メトリクス・トレースを OTLP で送る。確認ごとのスパン（`monitor.check`）、確認の数と応答時間の分布、停止・復旧の数、拠点の健全性の変化の数、
  送った通知の数、監視・停止・障害・時刻を過ぎた監視・送れていない通知・動いているノード・健全でない拠点の数（ゲージ）。画面のブラウザの観測（RUM）

## 構成

| モジュール | 内容 |
|---|---|
| `modules/shared` | バックエンドと画面（Scala.js）で共有する API の型と入力の確かめ（circe） |
| `modules/core` | 監視の確かめ方（HTTP(S)・TCP・DNS・PING. JDK の機能だけで行う） |
| `modules/db` | PostgreSQL（doobie・Flyway）. 監視・結果・障害・通知の行列・ステータスページ・ノード |
| `modules/auth` | API トークンの接頭辞・秘密の値の暗号化の salt（OIDC などの中身は sbirds-lib の auth） |
| `modules/devidp` | 開発・テスト用の簡易 OIDC プロバイダ（中身は sbirds-lib の devidp） |
| `modules/api` | REST API・MCP・監視を回す裏の処理（Worker）・通知の送信 |
| `modules/app` | 起動（設定・HTTP サーバー・画面の配信） |
| `modules/web` | 画面（Scala.js + Scalactive） |
| `frontend` | Vite（画面のビルド）、PWA の manifest と Service Worker |

## 開発

必要なもの: JDK 21、sbt、Node.js、Docker。

```bash
docker compose -f docker-compose.dev.yml up -d --wait   # PostgreSQL（:54389。テスト用の DB skotare_test も作る）
scripts/dev-restart.sh                                   # バックエンドを :8114 で起動（DEV_LOGIN=true。名前だけでログインできる）
(cd frontend && npm install && npm run dev)              # 画面の開発サーバー http://localhost:5196（/api・/auth・/mcp は :8114 へ中継）
sbt ~web/fastLinkJS                                      # 画面の変更を再リンクし続ける（ブラウザを再読み込みすると反映）
```

**OIDC で試す**: 開発用の IdP（:9097）を起動し、バックエンドを OIDC の設定で起動し直します。

```bash
scripts/dev-idp.sh
OIDC=dev scripts/dev-restart.sh
```

**複数の拠点で試す**: `scripts/dev-restart.sh` のノードは拠点 `tokyo` です。別の拠点のノードを足します（同じ DB を使う）。

```bash
scripts/dev-node.sh osaka 8115
```

テスト（確かめ方・拠点の判定の単体テスト、開発用 IdP と手元の HTTP サーバーを使った、ログインから停止・復旧・通知・複数の拠点の判定・ステータスページ・MCP までの通しのテスト）:

```bash
sbt test
```

**DB マイグレーション**（`modules/db/src/main/resources/db/migration`）: スキーマを変えるときは新しい番号の `V<n>__*.sql` を追加し、適用済みのファイルは書き換えないでください。

## 配布・起動

```bash
scripts/dist.sh                                    # dist/lib（jar）と dist/static（画面）
docker build -f deploy/docker/app.Dockerfile -t skotare .
```

複数台で動かすときは、同じ DB を指すノードを並べ、前にロードバランサーを置きます（例: `docker-compose.ha.yml`）。
ノードごとに `NODE_NAME` を変えると、確認の結果にどのノードが確かめたかが残ります。
別の場所（データセンター・クラウドのリージョン・回線）に置くノードには、場所ごとに `REGION` を付けます。拠点ごとに2台以上置くと、
1台が止まっても拠点の確認は続きます。拠点は3つ以上にすると、1つの拠点の故障を多数決で見分けられます（2つでは、両方が停止と見たときだけ停止）。
それぞれの拠点の外への出口（回線・DNS）を確かめるため、確かに動いている外のサイトを2〜3件「基準点」にしておくと確実です。
DB はすべての拠点から届く場所に置き、冗長にします（DB に届かない拠点は心拍が止まり、「停止」と判定されます）。

| 環境変数 | 内容 |
|---|---|
| `DATABASE_URL` / `DATABASE_USER` / `DATABASE_PASSWORD` | PostgreSQL 14 以降（`jdbc:postgresql://…`） |
| `DATABASE_POOL_SIZE` | DB の接続の数（既定 `CHECK_CONCURRENCY` + 6） |
| `PUBLIC_URL` | 公開 URL（`https://` なら Cookie に Secure が付く。通知の本文のリンクにも使う） |
| `OIDC_ISSUER` / `OIDC_CLIENT_ID` / `OIDC_CLIENT_SECRET` | OIDC。IdP には Redirect URI `${PUBLIC_URL}/auth/callback`、ログアウト後の戻り先 `${PUBLIC_URL}/` を登録する |
| `OIDC_SCOPES` | 要求するスコープ（既定 `openid profile email`） |
| `OIDC_GROUPS_CLAIM` / `OIDC_ALLOWED_GROUPS` | 所属グループのクレーム名（既定 `groups`）と、ログインを許すグループ（カンマ区切り。OIDC を使うときは必須） |
| `OIDC_ALLOW_ANY_USER` | `true` なら `OIDC_ALLOWED_GROUPS` なしで IdP のだれでもログインできる（監視を通して内部のネットワークを調べられるので、IdP を自分の組織に限るときだけ） |
| `SESSION_DAYS` | ログインの有効日数（既定 14） |
| `HTTP_PORT` | 待ち受けるポート（既定 8080） |
| `BIND_HOST` | 待ち受けるアドレス（既定 `0.0.0.0`。`DEV_LOGIN` のときは既定 `127.0.0.1`） |
| `NODE_NAME` | このノードの名前（既定 ホスト名） |
| `REGION` | このノードの拠点の名前（既定 `default`。同じ場所のノードは同じ名前にする。例 `tokyo`・`osaka`） |
| `CHECK_CONCURRENCY` | 1台で同時に確かめる数（既定 16、1〜256） |
| `PROBE_ALLOW_INTERNAL` | 監視してよい内部のアドレス（ホスト名・IP アドレス・CIDR をカンマか空白で区切る。例 `10.1.0.0/16,intra.example.internal`）。既定は空で、内部のアドレス（ループバック・プライベート・リンクローカル〈クラウドのメタデータ〉など）は監視できない。ホスト名は完全一致で、名前を引いた結果は確かめない |
| `NOTIFY_ALLOW_INTERNAL` | 通知（Webhook・stui の受信 Webhook・skereru の URL）を送ってよい内部のアドレス（書き方は `PROBE_ALLOW_INTERNAL` と同じ）。既定は空。`STUI_URL`・`STUI_INTERNAL_URL` のホストは書かなくても許す |
| `APP_SECRET` | 通知先の秘密の値（stui の Webhook の URL・skereru の APIキー）を暗号化する鍵（16文字以上。全ノードで同じ値にする。通知先を使うときは必須） |
| `STUI_URL`・`STUI_CLIENT_ID`・`STUI_CLIENT_SECRET` | stui との連携（通知先に stui のチャンネルを選ぶ）。stui の `OAUTH_CLIENTS` に skotare を登録し（`redirectUri` は `{PUBLIC_URL}/api/integrations/stui/callback`、`scopes` は `["read","write"]`）、その id・secret を入れる。`STUI_INTERNAL_URL` はサーバーから呼ぶ URL（省くと `STUI_URL`） |
| `OAUTH_CLIENTS` | skotare を利用者の権限で使うサービス（OAuth のクライアント）。`[{"id":"skakapo","secret":"…（16文字以上）","redirectUri":"https://…/api/connections/callback","name":"skakapo","scopes":["read","write"]}]`（`scopes` を省くと `read` だけ） |
| `RATE_LIMIT_AUTH` / `RATE_LIMIT_PUBLIC` | IP ごとの1分あたりの回数の上限。ログインと OAuth の認可の画面（`/auth/…`・`/oauth/authorize`、既定 20）と、OAuth のトークンの発行・失効（`/oauth/token`・`/oauth/revoke`）・ステータスページ（`/api/public/…`）（既定 120）。0 で制限しない |
| `TRUSTED_PROXIES` | リバースプロキシの IP か CIDR（カンマ区切り）。そこから来た要求だけ `X-Forwarded-For` から利用者の IP を読む |
| `STATIC_DIR` | ビルド済みの画面（`dist/static`） |
| `OTEL_EXPORTER_OTLP_ENDPOINT` / `OTEL_EXPORTER_OTLP_HEADERS` | ログ・メトリクス・トレースの送り先（skorimako の URL と `Authorization=Bearer sko_…`（ingest のキー））。未設定なら送らない |
| `LOG_FORMAT` / `LOG_LEVEL` | 標準出力のログの形（`text`（既定）・`json`）と水準（既定 `INFO`）。ログにはトレースの ID と操作したユーザーが付く |
| `SKORIMAKO_RUM_TOKEN` / `SKORIMAKO_RUM_URL` / `SKORIMAKO_RUM_SAMPLE_RATE` | 画面のブラウザの観測（skorimako の RUM）。トークンは skorimako の管理の画面で登録したアプリのもの |
| `VAPID_SUBJECT` | プッシュ通知（PWA）の連絡先（`mailto:…` か `https://…`。省略すると `PUBLIC_URL`）。VAPID の鍵は初回の起動で作って DB に保存する |
| `DEV_LOGIN` | 開発用のログインを有効にする（OIDC を設定せず、`PUBLIC_URL` が localhost の場合のみ） |

**PING について**: OS の `ping` コマンドを使います。コンテナでは ICMP を送れるように、`NET_RAW` の権限を付けるか、
カーネルの `net.ipv4.ping_group_range` で一般ユーザーの ICMP を許してください（`docker-compose.ha.yml` の例を参照）。

## 兄弟のサービスとの連携

skotare は、同じ技術で作っている兄弟プロジェクト（[stui](https://github.com/Ondeoma/stui)・[skereru](https://github.com/Ondeoma/skereru)・
[skorimako](https://github.com/Ondeoma/skorimako) など）と連携できます。どれも任意で、使わなくても skotare だけで動きます。

- **stui**: skotare の「通知先」で stui と連携し（OAuth）、通知先を追加するときに、自分が参加しているチャンネルを一覧から選びます。
  通知は、その通知先を作った人の連携（トークン）で、stui のボット「skotare」として投稿します（stui の `POST /api/apps/notify`）。
  作った人が連携をやめたりチャンネルを抜けたりすると送れなくなるので、ほかの人が通知先を開いてチャンネルを選び直します（その人の連携で投稿するようになります）。
  自分がチャンネルの管理者なら、「受信 Webhook を作って送る」も選べます（skotare が連携のトークンでそのチャンネルに受信 Webhook を作り、その URL で送ります。
  作ったあとは、連携をやめても届きます。通知先を消したり送り方を替えたりすると、作った Webhook も消します）。
  連携を使わず、stui のチャンネルの設定 →「受信 Webhook」で作った URL を登録することもできます。
- **skereru**: skereru の「設定 → APIキー・MCP」で send のスコープの APIキーを発行し、skotare の「通知先」に skereru の URL・差出人・宛先と一緒に登録します。
- **skorimako**: 上の `OTEL_*`・`SKORIMAKO_RUM_*` を設定します。
- **skakapo**（AI エージェント）: skotare を OAuth の提供側として使います。`OAUTH_CLIENTS` に skakapo を登録し（`redirectUri` は skakapo の `…/api/connections/callback`）、
  skakapo には認可の URL `${PUBLIC_URL}/oauth/authorize`、トークンの URL `${PUBLIC_URL}/oauth/token`（クライアントの認証は Basic）、MCP の URL `${PUBLIC_URL}/mcp` を設定します。
  認可コード + PKCE（S256 のみ）で、スコープは `read`（参照のみ）と `write`（読み書き）。アクセストークンは1時間で切れ、`refresh_token` で替えます
  （refresh token は使うたびに替わり、最後の更新から90日使わなければ切れます）。利用者は「設定 → 連携しているサービス」で連携を確かめ、取り消せます。

## セキュリティ

- 監視（HTTP・TCP・PING・DNS のサーバー）と通知は、ログインした人が指定した相手に、skotare のサーバーからつなぎます。
  内部のアドレス（ループバック・プライベート・リンクローカル〈クラウドのメタデータ〉など）へは、既定ではつなぎません。
  社内のサーバーを監視する・社内へ通知するときは、管理者が `PROBE_ALLOW_INTERNAL`・`NOTIFY_ALLOW_INTERNAL` に相手（ホスト名・CIDR）を書きます。
  名前を引いた結果のすべてのアドレスで確かめ、HTTP のリダイレクトは行き先を1回ごとに確かめます（5回まで）。通知はリダイレクトを追いません。
  ログインできる人は `OIDC_ALLOWED_GROUPS` で絞ってください（未設定では起動しません。絞らないときは `OIDC_ALLOW_ANY_USER=true` を明示）
- 一覧・通知・ログ・プッシュ通知に出す監視先の URL からは、利用者名・パスワードとクエリを除きます
- 無効にしたユーザーと、ログアウトした端末にはプッシュ通知を送りません
- ホスト名は英数字・`.`・`-`（と IP アドレス）だけを受け付け、`ping` はシェルを通さずに引数で渡します。ヘッダーに改行は入れられません
- 通知先の秘密の値は `APP_SECRET` の鍵で暗号化して保存し、画面・API には返しません。種類や送り先を変えるときは入れ直してもらいます
- 監視の設定（送るヘッダー・本文を含む）は、ログインした人と API トークン（参照のみを含む）から見えます。監視先の認証に使う値は、監視専用の権限の小さいものにしてください
- 本文の正規表現は200文字まで、照合は本文の先頭 64KB に 0.5 秒までにしています（時間のかかる正規表現でサーバーを止めさせない）
- ログインは state をブラウザの Cookie にも結び付けて確かめます（ログイン CSRF 対策）。開発用のログイン（`DEV_LOGIN`）のときは 127.0.0.1 でだけ待ち受けます
- ステータスページは監視の URL・ホスト・内部の名前を出しません
- OAuth の認可コード（60秒・1回だけ）・アクセストークン・refresh token もハッシュだけを保存します。許可の画面は Cookie と hidden の値の照合で CSRF を防ぎます
- セッション・API トークンはハッシュだけを保存します。画面からの更新には `X-Skotare` ヘッダーを要求します（CSRF 対策）
