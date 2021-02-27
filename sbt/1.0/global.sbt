Compile / packageDoc / publishArtifact := false

Compile / packageSrc / publishArtifact := false

target := baseDirectory.value / ".build"

taskTemporaryDirectory := target.value / "task-temporary-directory"

disablePlugins(plugins.JUnitXmlReportPlugin)

testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-oNCXEHLOPQRMWS")

testOptions += Tests.Argument(
  TestFramework("com.pagero.sbt.testing.testng.Framework"),
  "-oNCXHL"
)

Global / semanticdbEnabled := true

showSuccess := false

Global / excludeLintKeys += showSuccess
