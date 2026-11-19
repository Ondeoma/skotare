package skotare.auth

/** skotare の API トークン（見分けやすいよう skotare_ で始める）. */
object ApiTokens extends sbirds.auth.ApiKeys("skotare_")

/** skotare の秘密の値の暗号化（鍵の salt は skotare 固有. 変えると保存済みの値を復号できなくなる）. */
object SecretBox {
  def apply(secret: String): sbirds.auth.SecretBox = new sbirds.auth.SecretBox(secret, "skotare-secret-box:")
}

type SecretBox = sbirds.auth.SecretBox
