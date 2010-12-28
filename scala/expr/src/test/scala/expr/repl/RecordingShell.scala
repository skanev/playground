package expr.repl

class RecordedShell(playback: String*) extends Shell {
  var inputs = playback.toArray
  var outputBuffer = ""

  def read(): String = {
    if (inputs.isEmpty)
      throw new IllegalStateException("Inputs already consumed")

    val result = inputs(0)
    inputs = inputs.drop(1)
    return result
  }

  def write(output: String) = outputBuffer += output

  def messages = outputBuffer.split("\n")
  def lastMessage = messages.last
  def messageBeforeExit = messages.reverse(1)
}
