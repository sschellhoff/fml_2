plugins {
    kotlin("jvm") version "2.0.0"
}

group = "de.sschellhoff"
version = "1.0-SNAPSHOT"

repositories {
    mavenCentral()
}

dependencies {
    implementation("org.ow2.asm:asm:7.0")
    implementation("org.ow2.asm:asm-util:7.0")
    testImplementation(kotlin("test"))
}

tasks.test {
    useJUnitPlatform()
}
kotlin {
    jvmToolchain(21)
}