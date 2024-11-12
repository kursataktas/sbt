/*
 * sbt
 * Copyright 2011 - 2018, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * Licensed under Apache License 2.0 (see LICENSE)
 */

package sbt

import sbt.internal.Action
import sbt.internal.util.Types.const
import sbt.internal.util.{ AttributeKey, AttributeMap }
import ConcurrentRestrictions.{ Tag, TagMap, tagsKey }
import sbt.util.Monad

/**
 * Combines metadata `attributes` and a computation `work` to define a task.
 */
final case class Task[A](attributes: AttributeMap, post: A => AttributeMap, work: Action[A])
    extends TaskId[A]:
  override def toString = name.getOrElse(s"Task($attributes)")
  override def hashCode = (attributes, post).hashCode

  def name = attributes.get(Task.Name)
  def description = attributes.get(Task.Description)
  def get[B](key: AttributeKey[B]): Option[B] = attributes.get(key)
  def getOrElse[B](key: AttributeKey[B], default: => B): B = attributes.getOrElse(key, default)

  def setName(name: String): Task[A] = set(Task.Name, name)
  def setDescription(description: String): Task[A] = set(Task.Description, description)
  def set[A](key: AttributeKey[A], value: A) = copy(attributes = this.attributes.put(key, value))

  def postTransform(f: (A, AttributeMap) => AttributeMap): Task[A] = copy(post = a => f(a, post(a)))

  def tag(tags: Tag*): Task[A] = tagw(tags.map(t => (t, 1))*)
  def tagw(tags: (Tag, Int)*): Task[A] =
    val tgs: TagMap = get(tagsKey).getOrElse(TagMap.empty)
    val value = tags.foldLeft(tgs)((acc, tag) => acc + tag)
    set(tagsKey, value)
  def tags: TagMap = get(tagsKey).getOrElse(TagMap.empty)
end Task

object Task:
  import sbt.std.TaskExtra.*

  def apply[A](work: Action[A]): Task[A] =
    new Task[A](AttributeMap.empty, defaultAttributeMap, work)

  def apply[A](attributes: AttributeMap, work: Action[A]): Task[A] =
    new Task[A](attributes, defaultAttributeMap, work)

  val Name = AttributeKey[String]("name")
  val Description = AttributeKey[String]("description")
  val defaultAttributeMap = const(AttributeMap.empty)

  given taskMonad: Monad[Task] with
    type F[a] = Task[a]
    override def pure[A1](a: () => A1): Task[A1] = toTask(a)

    override def ap[A1, A2](ff: Task[A1 => A2])(in: Task[A1]): Task[A2] =
      multT2Task((in, ff)).mapN { case (x, f) =>
        f(x)
      }

    override def map[A1, A2](in: Task[A1])(f: A1 => A2): Task[A2] = in.map(f)
    override def mapN[A1 <: Tuple, A2](t: Tuple.Map[A1, Task])(f: A1 => A2): Task[A2] = t.mapN(f)
    override def flatMap[A1, A2](in: F[A1])(f: A1 => F[A2]): F[A2] = in.flatMap(f)
    override def flatten[A1](in: Task[Task[A1]]): Task[A1] = in.flatMap(identity)
end Task
