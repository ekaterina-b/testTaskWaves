package models

import java.util.UUID

import com.sun.jmx.snmp.Timestamp
import models.OperationType.OperationType
import models.SecuritiesType.SecuritiesType

case class Order (name: String,
                  operation: OperationType,
                  securitiesType: SecuritiesType,
                  cost:Int,
                  count: Int,
                  id: UUID)




