<script lang="scala">
// 最近の判定の小さな帯（確認の間隔ごとの区間. 古い順. 緑は正常・赤は停止）
case class Props(results: List[Boolean])

def cls(ok: Boolean): String = if (ok) "tick ok" else "tick bad"
</script>

<template>
  <span class="ticks"><span v-for="r in props.results" :class="cls(r)"></span></span>
</template>

<style scoped>
.ticks { display: inline-flex; gap: 2px; align-items: stretch; height: 18px; }
.tick { width: 4px; border-radius: 2px; }
.tick.ok { background: var(--up); }
.tick.bad { background: var(--down); }
</style>
