# 開発用の環境変数（source scripts/dev-env.sh）。docker-compose.dev.yml の DB を使う
export DATABASE_URL=jdbc:postgresql://localhost:54389/skotare DATABASE_USER=skotare DATABASE_PASSWORD=skotare
# 画面は Vite の開発サーバー（:5196. /api・/auth・/mcp を :8114 へ中継する）から開くので、公開 URL もそちらにする
export PUBLIC_URL=${PUBLIC_URL:-http://localhost:5196} HTTP_PORT=${HTTP_PORT:-8114}
# 拠点の名前（scripts/dev-node.sh で別の拠点のノードを足して、複数の拠点を試せる）
export REGION=${REGION:-tokyo}
# 認証: OIDC=dev なら開発用の IdP（scripts/dev-idp.sh、:9097）で OIDC のログインを試す。それ以外は名前だけの開発用ログイン
if [ "${OIDC:-}" = "dev" ]; then
  export OIDC_ISSUER=http://localhost:9097 OIDC_CLIENT_ID=skotare OIDC_CLIENT_SECRET=skotare-dev-secret-0123456789 OIDC_ALLOW_ANY_USER=true
  unset DEV_LOGIN
else
  export DEV_LOGIN=true
  unset OIDC_ISSUER OIDC_CLIENT_ID OIDC_CLIENT_SECRET
fi
# skotare を利用者の権限で使うサービス（OAuth のクライアント. 開発用の値. skakapo の画面は :5199）
export OAUTH_CLIENTS='[{"id":"skakapo","secret":"skakapo-dev-secret-0123456789","redirectUri":"http://localhost:5199/api/connections/callback","name":"skakapo","scopes":["read","write"]}]'
# 秘密の値の暗号化の鍵（開発用. 本番では十分に長い乱数にする）
export APP_SECRET=${APP_SECRET:-skotare-dev-app-secret-0123456789}
