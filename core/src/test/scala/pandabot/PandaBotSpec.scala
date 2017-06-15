package pandabot

import org.scalatest.{ EitherValues, FlatSpec, Matchers }
import org.scalatest.mockito.MockitoSugar

/**
 * Specification trait stack for Panda bot tests.
 */
abstract class PandaBotSpec extends FlatSpec with Matchers with EitherValues with MockitoSugar