package skotare.devidp

import cats.effect.*

/** 開発用の OIDC プロバイダ（本番では使わない. 中身は sbirds-lib の devidp）. */
object DevIdpMain extends IOApp.Simple {
  def run: IO[Unit] = sbirds.devidp.DevIdp.serve(9097, "skotare", "skotare-dev-secret-0123456789")
}
