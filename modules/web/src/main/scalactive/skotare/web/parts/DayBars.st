<script lang="scala">
import skotare.web.*
import skotare.shared.*

// 日ごとの稼働率の帯（古い日が左. 色: 99.9% 以上・95% 以上・それ未満・確認なし）
case class Props(days: List[DayBar])

def tip(d: DayBar): String = s"${d.day}: ${d.uptime.fold("確認なし")(u => Fmt.pct(Some(u)))}"
def first(props: Props): String = props.days.headOption.map(_.day).getOrElse("")
</script>

<template>
  <div class="days">
    <span v-for="d in props.days" :class="Fmt.barClass(d.uptime)" :title="tip(d)"></span>
  </div>
  <div class="days-legend muted small"><span>{{ first(props) }}</span><span>今日</span></div>
</template>

<style scoped>
.days { display: flex; gap: 2px; height: 30px; align-items: stretch; }
.days-legend { display: flex; justify-content: space-between; margin-top: .2rem; }
</style>
