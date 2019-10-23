package scroll.internal

import scala.collection.mutable

trait PrimaryKeyed {
  val key: String
}

trait ToggableCompartment[T <: ToggableCompartment[T]] extends Compartment {
  var enabled: Boolean = false
  val enabledSubCompartments: mutable.HashSet[T] = new mutable.HashSet()

  def parentCompartment: T

  def enable(): Boolean = {
    enabled = true
    parentCompartment.enabledSubCompartments.add(this.asInstanceOf[T])
  }

  def disable(): Boolean = {
    enabled = false
    enabledSubCompartments.foreach(t => t.disable())
    parentCompartment.enabledSubCompartments.remove(this.asInstanceOf[T])
  }
}