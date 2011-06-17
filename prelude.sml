

datatype 'seriously don't_call_print = It'sNoGoodForYou of 'seriously don't_call_print
exception WeUseStdOutToCommunicate
fun print (t : 'really don't_call_print) : 'actually don't_call_print = 
    raise WeUseStdOutToCommunicate

fun eprint s = TextIO.output(TextIO.stdErr, s)
