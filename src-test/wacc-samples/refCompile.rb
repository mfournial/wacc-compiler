# comand line script that sends an HTTPS request to the WACC compiler web interface and interprets the JSON response

require "rest-client"
require "json"
require "optparse"


ARGV << "-h" if ARGV.empty?

$options = {}
$opts = []
OptionParser.new do |opts|
  opts.banner = "Usage: #{$0} [options] <target.wacc> \n  options:"

  $options[:parse_only] = false
  opts.on("-p", "--parse_only",
    "Parse only. Check the input file for syntax errors and generate an AST.") do
    $opts << "-p" 
    $options[:parse_only] = true
  end

  $options[:semantic_check] = false
  opts.on("-s", "--semantic_check",
    "Semantic check. Parse the file for syntax and semantic errors and generate an AST." ) do
    $opts << "-s" 
    $options[:semantic_check] = true
  end

  $options[:print_ast] = false
  opts.on("-t", "--print_ast",
    "View AST. Display AST generated by the parser." ) do
    $opts << "-t" 
    $options[:print_ast] = true
  end

  $options[:print_asm] = false
  opts.on("-a", "--print_asm",
    "View Assembly. Display ARM assembly code generated by the code generator." ) do
    $opts << "-a" 
    $options[:print_asm] = true
  end

  $options[:stack] = false
  opts.on("-S", "--stack",
    "Generate code using stack implementation," ) do
    $opts << "-S" 
    $options[:stack] = true
  end

  $options[:optimise] = -1
  opts.on("-o [level]", "--optimise [LEVEL]", Integer,
    "Optimise the code using given level of optimisation (Each level runs previous levels).
\t0 -- Expression Ordering
\t1 -- Constant Folding
\t2 -- Constant Propagation
\t3 -- Flow Analysis
\t4 -- Redundant Code Elimination
\t5 -- Peephole Optimisations
    " ) do |integer|
    $options[:optimise] = integer || 0
    $opts << "-o" << $options[:optimise]
  end

  $options[:execute] = false
  opts.on("-x", "--execute",
    "Execute. Assemble and Emulate the generated ARM code and display its output." ) do
    $opts << "-x" 
    $options[:execute] = true
  end

  $options[:directory] = false
  opts.on("-d", "--directory",
    "Give directory of wacc files." ) do
    $options[:directory] = true
  end

  opts.on_tail("-h", "--help",
    "Show this message") do
    puts opts
    puts ""
    puts "  target.wacc: path to wacc program file to compile (or target directory if -dir option set)"
    exit
  end

end.parse!

# set up empty array of file_paths to process
files = []
puts ARGV[0]
if $options[:directory] then
  # add the results of a search to the array
  files += Dir.glob(File.join(ARGV[0], "**", "*.wacc"))
else
  # just add the target file to the array
  files << ARGV[0]
end

# check that there is at least one file to process
if files == [] then
  puts "Error: no file(s) targeted"
  exit 1
end

# now process each target file
files.each do |file_path|
  puts "calling the reference compiler on #{file_path}"
  
  #get stdin from the user to pass to the reference compiler
  stdin = ""
  if $options[:execute] then
    puts "please provide a stdin stream to use when executing the program:"
    stdin = STDIN.gets
  end

  RestClient.post("https://teaching.doc.ic.ac.uk/wacc_compiler/run.cgi", 
                    :stdin => stdin,
                    :options => $opts,
                    :testfile => File.new(file_path)
                   ) do |response|
    json = JSON.load(response)
    puts "-- Test: #{json['test']}"
    puts ""
    puts "-- Uploaded file: "
    puts "---------------------------------------------------------------"
    puts json['upload']
    puts "---------------------------------------------------------------"
    puts ""
    puts "-- Compiler Output:"
    puts json['compiler_out']
    puts ""
  end
  
end
