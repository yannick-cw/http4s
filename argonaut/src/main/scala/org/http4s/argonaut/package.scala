package org.http4s

import _root_.argonaut._

package object argonaut extends ArgonautInstances {
  def prettyParams: PrettyParams =
    Argonaut.nospace
}
