#!/usr/bin/env bash
# 手元で、別の拠点のノードをもう1台動かす（複数の拠点からの確認を試す）.
#   scripts/dev-node.sh osaka 8115
# sbt で動いているノード（scripts/dev-env.sh. 拠点 tokyo・:8114）と同じ DB を使う. 画面は :5196 から開く.
set -euo pipefail
cd "$(dirname "$0")/.."
region=${1:?拠点の名前を指定してください（例: osaka）}
port=${2:-8115}
cp=target/dev-classpath.txt
if [ ! -s "$cp" ] || [ -n "$(find modules -name '*.scala' -newer "$cp" -print -quit)" ]; then
  sbt -batch -no-colors "export app/Runtime/fullClasspath" | tail -1 > "$cp"
fi
export HTTP_PORT=$port REGION=$region NODE_NAME=${NODE_NAME:-$region-1}
. scripts/dev-env.sh
exec java -cp "$(cat "$cp")" skotare.app.Main
