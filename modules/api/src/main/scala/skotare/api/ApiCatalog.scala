package skotare.api

/** REST API の一覧（MCP の list_api_endpoints と GET /api/endpoints）. */
object ApiCatalog {
  final case class Endpoint(
    method: String,
    path: String,
    description: String)
  private val monitorBody =
    "{name, kind: http|tcp|dns|ping, config: {url, method, headers: [{name, value}], body, expectStatus（例 200-299,301）, " +
      "bodyContains, bodyNotContains, bodyRegex, followRedirects, certExpiryDays, host, port, recordType: A|AAAA|CNAME|MX|TXT|NS, dnsServer, expect}, " +
      "intervalSec（20〜86400）, timeoutMs（500〜60000）, retries（0〜10）, enabled, description, canary, confirmRegions（0〜20. 0 は過半数）}"
  val all: List[Endpoint] = List(
    Endpoint("GET", "/api/me", "自分の情報"),
    Endpoint("PATCH", "/api/me", "自分の名前・タイムゾーンを変える {name?, timezone?}"),
    Endpoint("GET", "/api/summary", "全体の様子（正常・停止・一時停止・未確認の数、続いている障害の数、動いているノード）"),
    Endpoint("GET", "/api/nodes", "動いているノード（冗長構成. region は拠点）"),
    Endpoint("GET", "/api/regions", "拠点と、その健全性（ok・suspect（skotare 側の異常の疑い）・offline）"),
    Endpoint("GET", "/api/monitors", "監視の一覧（状態・稼働率・最近の結果）"),
    Endpoint("POST", "/api/monitors", s"監視を作る $monitorBody"),
    Endpoint("POST", "/api/monitors/test", "監視の設定を、保存せずに今すぐ試す（本文は作成と同じ）"),
    Endpoint("GET", "/api/monitors/{id}", "監視の詳細（最近の100回の結果・障害・90日の日ごとの稼働率）"),
    Endpoint("PUT", "/api/monitors/{id}", "監視を変える（本文は作成と同じ）"),
    Endpoint("DELETE", "/api/monitors/{id}", "監視を削除する（結果・障害も消える）"),
    Endpoint("POST", "/api/monitors/{id}/check", "すぐに確かめる"),
    Endpoint("GET", "/api/incidents?open=true&monitor={id}&limit=", "障害の一覧（新しい順）"),
    Endpoint("GET", "/api/notifiers", "通知先の一覧（秘密の値は返さない）"),
    Endpoint(
      "POST",
      "/api/notifiers",
      "通知先を作る {name, kind: stui|skereru|webhook, secret（stui の受信 Webhook の URL・skereru の APIキー・Webhook の URL）, " +
        "config: {baseUrl（skereru の URL）, from, to: [メール], channelId（stui のチャンネル. 指定すると secret は要らず、呼んだ人の stui との連携で投稿する）}, allMonitors, monitorIds, enabled}"
    ),
    Endpoint("PUT", "/api/notifiers/{id}", "通知先を変える（secret は変えるときだけ）"),
    Endpoint("DELETE", "/api/notifiers/{id}", "通知先を削除する"),
    Endpoint("POST", "/api/notifiers/{id}/test", "通知先に試しに送る"),
    Endpoint("GET", "/api/integrations", "外部のサービスとの連携（stui）と、自分が連携しているか"),
    Endpoint("GET", "/api/integrations/stui/channels", "通知先に選べる stui のチャンネル（自分が参加しているもの. stui と連携している必要がある）"),
    Endpoint("GET", "/api/pages", "ステータスページの一覧"),
    Endpoint(
      "POST",
      "/api/pages",
      "ステータスページを作る {slug, title, description, notice（お知らせ）, public, items: [{monitorId, displayName?}]}"
    ),
    Endpoint("PUT", "/api/pages/{id}", "ステータスページを変える（本文は作成と同じ）"),
    Endpoint("DELETE", "/api/pages/{id}", "ステータスページを削除する"),
    Endpoint("GET", "/api/public/status/{slug}", "ステータスページの内容（ゲストに見せるもの. 公開のページはログインなしで見られる）")
  )
}
