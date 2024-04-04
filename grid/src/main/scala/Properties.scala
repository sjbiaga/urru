package urru
package main


object Properties:

  object Validator:

    val optimized = "urru.di.validator.optimized"
    val plainjava = "urru.di.validator.plainjava"

  object CipherServer:

    val timeout = "urru.common.cipher.server.timeout"
    val host = "urru.common.cipher.server.host"
    val port = "urru.common.cipher.server.port"

  object DependencyInjection:

    val config = "urru.nest.config.mapper"
    val build = "urru.nest.config.builder"
    val guice = "urru.nest.di.guice"
    val macwire = "urru.nest.di.macwire"

  object WebSimple:

    val plainjava = "urru.web.simple.plainjava"
    val scheme = "urru.web.simple.scheme"
    val host = "urru.web.simple.host"
    val port = "urru.web.simple.port"

  object WebCue:

    val port = "urru.web.cue.port"
    val timeout = "urru.web.cue.timeout"
