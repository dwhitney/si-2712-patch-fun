
import cats.{~>,Id}
import cats.free.{Free,Inject}
import cats.data.Coproduct

sealed trait KVSADT[K,V,E] extends Product with Serializable
final case class Put[K,V](key: K, value: V) extends KVSADT[K,V,Unit]
final case class Get[K,V](key: K) extends KVSADT[K,V,V]

object KVSDSL {
  implicit def dsl[F[_], K, V](implicit I: Inject[KVSADT[K,V,?], F]): KVSDSL[F, K, V] = new KVSDSL[F, K, V]
}

class KVSDSL[F[_], K, V](implicit I: Inject[KVSADT[K,V,?], F]){

  type KVS[A] = Free[F, A]

  def put(key: K, value: V): KVS[Unit] =
    Free.inject[KVSADT[K,V,?], F](Put(key, value))

  def get(key: K): KVS[V] =
    Free.inject[KVSADT[K,V,?], F](Get(key))

}

class MapKVSInterpreter[K,V] extends (KVSADT[K,V,?] ~> Id){

  var map = Map[K,V]()

  def put(key: K, value: V): Unit =
    map = map + (key -> value)

  def get(key: K): V =
    map(key)

  def apply[A](fa: KVSADT[K,V,A]) = fa match {
    case Put(key, value) => put(key, value)
    case Get(key) => get(key).asInstanceOf[Id[A]]
  }

}

object MyService{

  type PRG[A] = Coproduct[KVSADT[String,Int,?],KVSADT[Int,String,?], A]

  def program(implicit KVSSI: KVSDSL[PRG,String,Int], KVSIS: KVSDSL[PRG,Int,String]): Free[PRG,String] = {
    for{
      _   <- KVSSI.put("one", 1)
      one <- KVSSI.get("one")
      _   <- KVSIS.put(one, "one")
      str <- KVSIS.get(one)
    } yield str
  }
}

object Main extends App{

  val SIMapInterpreter = new MapKVSInterpreter[String,Int]
  val ISMapInterpreter = new MapKVSInterpreter[Int,String]

  val coproductInterpreter: ~>[MyService.PRG, Id] = {
    new ~>[MyService.PRG,Id]{
      def apply[A](prg: MyService.PRG[A]): Id[A] = {
        prg.run.fold(
            kvssi => SIMapInterpreter(kvssi)
          , kvsis => ISMapInterpreter(kvsis)
        )
      }
    }
  }

  val prg = MyService.program

  println(prg.foldMap(coproductInterpreter))
  println(SIMapInterpreter.map)
  println(ISMapInterpreter.map)

}
