#!/usr/bin/env bash
# 開発用: 8114 番のサーバーを止めて、起動し直す（ログは $LOG、既定 /tmp/skotare-dev.log）。起動を待ってから戻る
# OIDC=dev scripts/dev-restart.sh で、開発用の IdP（scripts/dev-idp.sh）を使った OIDC のログインになる
cd "$(dirname "$0")/.."
. scripts/lib.sh
. scripts/dev-env.sh
LOG=${LOG:-/tmp/skotare-dev.log}
free_port "$HTTP_PORT"
run_sbt app/run "$LOG" 'listening on' 180
