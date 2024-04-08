package object urru:

  type Point = common.grid.Point

  extension(p: Boolean)
    inline def ->(q: => Boolean): Boolean = !p || q


package urru:

  package grid:

    package object shape:

      abstract trait At:
        val at: Point

      abstract trait By extends At:
        val by: Point

      abstract trait Color:
        val color: Int

      abstract trait Block:
        val block: Seq[Point]
        val min: (Int, Int)
        val max: (Int, Int)

      abstract trait Sides:
        val up: Seq[Seq[Int]]
        val down: Seq[Seq[Int]]
        val left: Seq[Seq[Int]]
        val right: Seq[Seq[Int]]

    object Implicits:

      import io.github.greenleafoss.mongo.GreenLeafJsonProtocol
      import spray.json.{ JsonFormat, RootJsonFormat }

      trait IdJsonProtocol extends GreenLeafJsonProtocol:
        implicit lazy val IdFormat: RootJsonFormat[Grid.Id] = jsonFormat2(Grid.Id.apply)

      object IdJsonProtocol extends IdJsonProtocol

////////////////////////////////////////////////////////////////////////////////

      trait CountersJsonProtocol extends GreenLeafJsonProtocol:
        implicit lazy val CountersFormat: JsonFormat[Game.Counters] =
          jsonFormat3(Game.Counters.apply)

      object CountersJsonProtocol extends CountersJsonProtocol

////////////////////////////////////////////////////////////////////////////////

      trait FeatureJsonProtocol extends GreenLeafJsonProtocol:
        import spray.json.{ JsString, JsValue }

        implicit def FeatureFormat: JsonFormat[Game.Feature] =
          new JsonFormat[Game.Feature]:
            def write(self: Game.Feature) = JsString(self.toString)

            def read(value: JsValue): Game.Feature = value match
              case it: JsString => Game.Feature.valueOf(it.value)
              case _ => ???

      object FeatureJsonProtocol extends FeatureJsonProtocol
