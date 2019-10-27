package scroll.internal

import scroll.internal.errors.SCROLLErrors.RoleNotFound
import scroll.internal.errors.SCROLLErrors.SCROLLError
import scroll.internal.support.DispatchQuery
import scroll.internal.util.{NamedMethodHandle, ReflectiveHelper}

import scala.reflect.ClassTag

/**
  * This Trait allows for implementing an objectified collaboration with a limited number of participating roles and a fixed scope.
  *
  * ==Overview==
  * Roles are dependent on some sort of context. We call them compartments. A typical example of a compartment is a university,
  * which contains the roles Student and Teacher collaborating in Courses. Everything in SCROLL happens inside of Compartments
  * but roles (implemented as standard Scala classes) can be defined or imported from everywhere. Just mix in this Trait
  * into your own specific compartment class or create an anonymous instance.
  *
  * ==Example==
  * {{{
  * val player = new Player()
  * new Compartment {
  *   class RoleA
  *   class RoleB
  *
  *   player play new RoleA()
  *   player play new RoleB()
  *
  *   // call some behaviour
  * }
  * }}}
  */
trait Compartment extends ICompartment {

  override def newPlayer[W <: AnyRef : ClassTag](obj: W): Player[W] = {
    require(null != obj)
    new Player(obj)
  }

  implicit class Player[W <: AnyRef : ClassTag](override val wrapped: W) extends IPlayer[W, Player[W]](wrapped) with SCROLLDynamic {

    override def applyDynamicNamed[E](name: String)(args: (String, Any)*)(implicit dispatchQuery: DispatchQuery = DispatchQuery.empty, tag: ClassTag[E]): Either[SCROLLError, E] =
      applyDynamic[E](name)(args.map(_._2): _*)(dispatchQuery, tag)

    override def applyDynamic[E](name: String)(args: Any*)(implicit dispatchQuery: DispatchQuery = DispatchQuery.empty, tag: ClassTag[E]): Either[SCROLLError, E] =
      applyDispatchQuery(dispatchQuery, wrapped).view.map { r: AnyRef =>
        (r, ReflectiveHelper.findMethod(r, name, args, tag.runtimeClass))
      }.collectFirst {
        case (r: AnyRef, Some(m: NamedMethodHandle)) => dispatch[E](r, m, args: _*)
      }.getOrElse(Left(RoleNotFound(wrapped.toString, name, args)))

    override def selectDynamic[E](name: String)(implicit dispatchQuery: DispatchQuery = DispatchQuery.empty): Either[SCROLLError, E] =
      applyDispatchQuery(dispatchQuery, wrapped).view.collectFirst {
        case r: AnyRef if ReflectiveHelper.hasMember(r, name) => Right(ReflectiveHelper.propertyOf[E](r, name))
      }.getOrElse(Left(RoleNotFound(wrapped.toString, name, Seq.empty[Any])))

    override def updateDynamic(name: String)(value: Any)(implicit dispatchQuery: DispatchQuery = DispatchQuery.empty): Unit =
      applyDispatchQuery(dispatchQuery, wrapped).view.
        find(ReflectiveHelper.hasMember(_, name)).
        foreach(ReflectiveHelper.setPropertyOf(_, name, value))

  }

}
