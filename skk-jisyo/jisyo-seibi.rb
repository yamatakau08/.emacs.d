skk_jisyo = ".skk-jisyo"

## print first entry which has some candidates
#  use this result | sort | uniq -D
#  puts out the duplicate entry
IO.foreach(skk_jisyo) do |line|
  puts "#{line.split.first}" if line.scan("/").size > 2
end
