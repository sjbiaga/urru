package urru

import pureconfig.*
import pureconfig.generic.derivation.default.*

import Config.*

final case class Config(urru: URRUConfig) derives ConfigReader


object Config:

  private lazy val config = ConfigSource.default.load[Config].right.get

  final case class URRUConfig(mongo: MongoConfig)
  final case class MongoConfig(host: String, port: Int, database: String)

  def apply(): Config = config
