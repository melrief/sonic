/*
 * Copyright (c) 2017 Mario Pastorelli (pastorelli.mario@gmail.com)
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package sonic

package object syntax {

  implicit final class SonicOps[A](a: A) {

    def ===(that: A): Test[Unit] = {
      if (a == that) Test.success
      else {
        // TODO: add diff when possible
        val msg =
          s"""=== Not Equals ===
             |$a
             |$that
          """.stripMargin
        Test.failWith(None, msg)
      }
    }
  }

}
