package model.entity.hmm

/**
 * data for testing the rain example
 * Created by cd on 31/01/15.
 */
object TestRainExample {

  val data =
    List(
      List("noumbrella", "dry"),
      List("noumbrella", "noumbrella", "dry"),
      List("noumbrella", "noumbrella", "noumbrella", "dry"),
      List("noumbrella", "umbrella", "noumbrella", "dry"),
      List("noumbrella", "umbrella", "noumbrella", "noumbrella", "dry"),
      List("noumbrella", "noumbrella", "noumbrella", "dry"),


      List("noumbrella", "umbrella", "rain"),
      List("noumbrella", "umbrella", "noumbrella", "dry"),
      List("noumbrella", "umbrella", "noumbrella", "noumbrella", "dry"),
      List("noumbrella", "umbrella", "noumbrella", "noumbrella", "umbrella", "rain"),
      List("noumbrella", "umbrella", "noumbrella", "umbrella", "rain"),

      List("umbrella", "rain"),
      List("umbrella", "umbrella", "rain"),
      List("umbrella", "umbrella", "umbrella", "rain"),


      List("noumbrella", "absent", "storm"),
      List("umbrella", "absent", "storm"),
      List("absent", "absent", "absent", "storm"),

      List("noumbrella", "absent", "umbrella", "rain"),
      List("noumbrella", "absent", "umbrella", "absent", "storm"),
      List("noumbrella", "absent", "absent", "absent", "umbrella", "rain"),
      List("absent", "absent", "absent", "absent", "storm")

    )

}
