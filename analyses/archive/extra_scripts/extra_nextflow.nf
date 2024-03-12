 
// this works but it is unclear how to use the logic to change the directive, which needs to be a the top
process testIterate{
       label 'tiny'
       input: 
         path x
       output: 
         stdout
       script:
       if("$x" =~ /scenario2/)
       """
echo 'Working on file $x'
"""
       else
	  """
echo 'Would have been working on file $x'
"""
}