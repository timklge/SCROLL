package scroll.internal.util

import java.lang.invoke.{MethodHandle, MethodHandles, MethodType}

import scala.collection.immutable.ArraySeq
import scala.reflect.ClassTag
import scala.reflect.classTag

final case class NamedMethodHandle(name: String, handle: MethodHandle)
final case class NamedMethodType(name: String, typ: MethodType)
final case class NamedField(name: String, getter: MethodHandle, setter: MethodHandle)

/**
  * Contains useful functions for translating class and type names to Strings
  * and provides helper functions to access common tasks for working with reflections.
  *
  * Querying methods and fields is cached.
  */
object ReflectiveHelper extends Memoiser {

  private[this] lazy val methodCache =
    buildCache[Class[_], Seq[NamedMethodHandle]](allMethods)

  private[this] lazy val methodsByNameCache =
    buildCache[(Class[_], String), Seq[NamedMethodHandle]]((t: (Class[_], String)) => cachedFindMethods(t._1, t._2))

  private[this] lazy val methodMatchCache =
    buildCache[(Class[_], String, MethodType), Option[NamedMethodHandle]]((t: (Class[_], String, MethodType)) => cachedFindMethod(t._1, t._2, t._3))

  private[this] lazy val fieldCache =
    buildCache[Class[_], Seq[NamedField]]((c: Class[_]) => allFields(c))

  private[this] lazy val fieldByNameCache =
    buildCache[(Class[_], String), NamedField]((t: (Class[_], String)) => cachedFindField(t._1, t._2))

  private[this] lazy val classNameCache =
    buildCache[String, String](cachedSimpleName)

  private[this] lazy val hasMemberCache =
    buildCache[(Class[_], String), java.lang.Boolean]((t: (Class[_], String)) => cachedHasMember(t._1, t._2))

  def addToMethodCache(c: Class[_]): Unit = methodCache.put(c, allMethods(c))

  def addToFieldCache(c: Class[_]): Unit = fieldCache.put(c, allFields(c))

  private[this] def simpleClassName(s: String, on: String) = if (s.contains(on)) {
    s.substring(s.lastIndexOf(on) + 1)
  } else {
    s
  }

  private[this] def cachedSimpleName(t: String): String = simpleClassName(simpleClassName(t, "."), "$")

  /**
    * Translates a Class or Type name to a String, i.e. removing anything before the last
    * occurrence of "<code>$</code>" or "<code>.</code>".
    *
    * @param t the Class or Type name as String
    * @return anything after the last occurrence of "<code>$</code>" or "<code>.</code>"
    */
  def simpleName(t: String): String = classNameCache.get(t)

  /**
    * Compares two class names.
    *
    * @param mani the first class name derived from a class manifest (e.g., from classTag) as String
    * @param that the second class name already as instance of Any
    * @return true iff both names are the same, false otherwise
    */
  def isInstanceOf(mani: String, that: AnyRef): Boolean =
    simpleName(that.getClass.toString) == simpleName(mani)

  /**
    * Compares two class names.
    *
    * @param mani the first class name derived from a class manifest (e.g., from classTag) as String
    * @param that the second class name already as String
    * @return true iff both names are the same, false otherwise
    */
  def isInstanceOf(mani: String, that: String): Boolean =
    simpleName(that) == simpleName(mani)

  /**
    * Compares two interfaces given as Array of its Methods.
    *
    * @param roleInterface  Array of Methods from the first interface
    * @param restrInterface Array of Methods from the second interface
    * @return true iff all methods from the restrInterface can be found in roleInterface, false otherwise
    */
  def isSameInterface(roleInterface: Seq[NamedMethodType], restrInterface: Seq[NamedMethodType]): Boolean =
    restrInterface.forall(method => roleInterface.exists(method.equals))

    def isSameInterface(roleA: Class[_], roleB: Class[_]): Boolean = {
      val typesA = allMethods(roleA).map(n => NamedMethodType(n.name, n.handle.`type`))
      val typesB = allMethods(roleB).map(n => NamedMethodType(n.name, n.handle.`type`))
      isSameInterface(typesA, typesB)
    }

  private[this] def cachedFindField(of: Class[_], name: String): NamedField =
    fieldCache.get(of).find(_.name == name).getOrElse({
      throw new RuntimeException(s"Field '$name' not found on '$of'!")
    })

  private[this] def findField(of: Class[_], name: String): NamedField =
    fieldByNameCache.get((of, name))

  private[this] def cachedFindMethods(of: Class[_], name: String): Seq[NamedMethodHandle] =
    methodCache.get(of).filter(_.name == name)

  private[this] def findMethods(of: Class[_], name: String): Seq[NamedMethodHandle] =
    methodsByNameCache.get((of, name))

  private[this] def allMethods(of: Class[_]): Seq[NamedMethodHandle] = {
    def getAccessibleMethods(c: Class[_]): Seq[NamedMethodHandle] = c match {
      case null => Seq.empty[NamedMethodHandle]
      case _ => ArraySeq.unsafeWrapArray(c.getDeclaredMethods.map(m => {
        m.setAccessible(true)
        NamedMethodHandle(m.getName, MethodHandles.lookup().unreflect(m))
      })).concat(getAccessibleMethods(c.getSuperclass))
    }

    getAccessibleMethods(of)
  }

  private[this] def allFields(of: Class[_]): Seq[NamedField] = {
    def accessibleFields(c: Class[_]): Seq[NamedField] = c match {
      case null => Seq.empty[NamedField]
      case _ => ArraySeq.unsafeWrapArray(c.getDeclaredFields.map(f => {
        f.setAccessible(true)

        NamedField(f.getName, MethodHandles.lookup().unreflectGetter(f), MethodHandles.lookup().unreflectSetter(f))
      })).concat(accessibleFields(c.getSuperclass))
    }

    accessibleFields(of)
  }


  private[this] def cachedFindMethod(on: Class[_], name: String, t: MethodType): Option[NamedMethodHandle] =
    findMethods(on, name).find(_.handle.`type`() == t)

  /**
    * Find a method of the wrapped object by its name and argument list given.
    *
    * @param on   the instance to search on
    * @param name the name of the function/method of interest
    * @param args the args function/method of interest
    * @return Some(Method) if the wrapped object provides the function/method in question, None otherwise
    */
  def findMethod(on: AnyRef, name: String, args: Seq[Any], returntype: Class[_]): Option[NamedMethodHandle] =
    methodMatchCache.get((on.getClass, name, MethodType.methodType(returntype, args.map(_.getClass).toArray)))

  private[this] def cachedHasMember(on: Class[_], name: String): java.lang.Boolean = {
    val fields = fieldCache.get(on)
    val methods = methodCache.get(on)
    fields.exists(_.name == name) || methods.exists(_.name == name)
  }

  /**
    * Checks if the wrapped object provides a member (field or function/method) with the given name.
    *
    * @param on   the instance to search on
    * @param name the name of the member (field or function/method)  of interest
    * @return true if the wrapped object provides the given member, false otherwise
    */
  def hasMember(on: AnyRef, name: String): Boolean = hasMemberCache.get((on.getClass, name))

  /**
    * Returns the runtime content of type T of the field with the given name of the wrapped object.
    *
    * @param on   the instance to search on
    * @param name the name of the field of interest
    * @tparam T the type of the field
    * @return the runtime content of type T of the field with the given name of the wrapped object
    */
  def propertyOf[T](on: AnyRef, name: String): T =
    findField(on.getClass, name).getter.invoke(on).asInstanceOf[T]

  /**
    * Sets the field given as name to the provided value.
    *
    * @param on    the instance to search on
    * @param name  the name of the field of interest
    * @param value the value to set for this field
    */
  def setPropertyOf(on: AnyRef, name: String, value: Any): Unit =
    findField(on.getClass, name).setter.invoke(on, value)

  /**
    * Returns the runtime result of type T of the given function and arguments by executing this function of the wrapped object.
    *
    * @param on   the instance to search on
    * @param name the function name of interest
    * @param args the arguments of the function of interest
    * @tparam T the return type of the function
    * @return the runtime result of type T of the function with the given name by executing this function of the wrapped object
    */
  def resultOf[T](on: AnyRef, name: NamedMethodHandle, args: Seq[AnyRef]): T = {
    val margs: Seq[AnyRef] = on +: args
    name.handle.invoke(on, margs).asInstanceOf[T]
  }

  /**
    * Returns the runtime result of type T of the function with the given name by executing this function of the wrapped object.
    *
    * @param on   the instance to search on
    * @param name the name of the function of interest
    * @tparam T the return type of the function
    * @return the runtime result of type T of the function with the given name by executing this function of the wrapped object
    */
  def resultOf[T](on: AnyRef, name: String): T =
    findMethods(on.getClass, name) match {
      case elem +: _ =>
        elem.handle.invoke(on).asInstanceOf[T]
      case Nil =>
        throw new RuntimeException(s"Function with name '$name' not found on '$on'!")
    }

  /**
    * Checks if the wrapped object is of type T.
    *
    * @param on the instance to search on
    * @tparam T the type to check
    * @return true if the wrapped object is of type T, false otherwise
    */
  def is[T <: AnyRef : ClassTag](on: AnyRef): Boolean =
    simpleName(on.getClass.toString) == simpleName(classTag[T].toString)
}


