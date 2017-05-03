package lectures.functions

import org.scalacheck.Gen
import org.scalatest.{Matchers, WordSpec}
import Authentication._
import AuthenticationData._
import org.scalatest.prop.PropertyChecks

/**
  * Авторизация - это очень важно, поэтому нам необходимо покрыть тестами ответсвенный за нее код
  * (lectures.functions.Authentication)
  *
  * Для этого
  * * * * уберите extends App у Authentication
  * * * * замените AuthenticationData.testUsers соответствующими генераторами
  * * * * напишите
  * * * * * 2 теста на authByCard
  * * * * * 2 теста на authByLP
  * * * * * 1 тест на их композицию
  *
  */
class AuthenticationTest extends WordSpec with PropertyChecks with Matchers {
  val unregCardUserGen: Gen[CardUser] = Gen zip(Gen.choose(0, 10000), Gen.choose(0, 10000)) map (c => CardUser(c._1, CardCredentials(c._2)))
  val regCardUserGen: Gen[CardUser] = Gen zip(Gen.choose(0, 10000), Gen.oneOf(registeredCards.toList)) map (c => CardUser(c._1, c._2))
  val unregLoginPasswordUserGen: Gen[LPUser] = Gen zip(Gen.choose(0, 10000), Gen.alphaStr, Gen.alphaStr) map (c => LPUser(c._1, LPCredentials(c._2, c._3)))
  val regLoginPasswordUserGen: Gen[LPUser] = Gen zip(Gen.choose(0, 10000), Gen.oneOf(registeredLoginAndPassword.toList)) map (c => LPUser(c._1, c._2))
  val anonUserGen: Gen[AnonymousUser] = Gen.const(AnonymousUser())

  "An user" which {
    "has registered card" should {
      "be successfully authenticated" in {
        forAll(regCardUserGen) {
          authByCard.isDefinedAt(_) shouldBe true
        }
      }
    }

    "has unregistred card" should {
      "be rejected" in {
        forAll(unregCardUserGen.filter(cardUser => !registeredCards.contains(cardUser.credentials))) {
          authByCard.isDefinedAt(_) shouldBe false
        }
      }
    }

    "has registred login and password" should {
      "be successfully authenticated" in {
        forAll(regLoginPasswordUserGen) {
          authByCard.isDefinedAt(_) shouldBe true
        }
      }
    }
    "has unknown login and password" should {
      "be rejected" in {
        forAll(unregLoginPasswordUserGen.filter(lpUser => !registeredLoginAndPassword.contains(lpUser.credentials))) {
          authByCard.isDefinedAt(_) shouldBe false
        }
      }
    }
    "has registered card or login and password" should {
      "be successfully authenticated" in {
        forAll(Gen.oneOf(regCardUserGen, regLoginPasswordUserGen)) {
          (authByCard orElse authByLP).lift(_).isDefined shouldBe true
        }
      }
    }
  }
}
