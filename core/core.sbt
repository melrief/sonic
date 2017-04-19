name := "sonic"

libraryDependencies ++= Seq(
  "eu.timepit"    %% "refined"    % "0.8.0",
  "org.typelevel" %% "cats"       % "0.9.0",
  "org.typelevel" %% "cats-free"  % "0.9.0",
  "io.monix"      %% "monix-eval" % "2.2.4",
  "io.monix"      %% "monix-cats" % "2.2.4",
  "org.scalatest" %% "scalatest"  % "3.0.1"  % "test"
)

autoCompilerPlugins := true

resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")
