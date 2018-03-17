package org.plan_explorer.model

import org.neo4j.kernel.impl.query.clientconnection.ClientConnectionInfo

object ConnectionInfo extends ClientConnectionInfo {
  override def protocol(): String = "w00t"

  override def asConnectionDetails(): String = "w00t"
}
