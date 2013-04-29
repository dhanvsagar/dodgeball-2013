module AsciiToVideoUtils
  extend self

  def convert_to_images(from, to) 
    STDOUT.puts "converting text to images from #{from} into #{to}"
    Dir.entries(from).each do |file|
      if file != "." && file != ".."
        name = file.pathmap("%n")
        `convert -size 640x480 xc:white -font Courier -pointsize 18 -fill black -gravity center -draw "text 0,0 '$(cat #{from}/#{file})'" #{to}/#{name}.gif`
      end
    end
  end

  def create_animation(from_folder, images_folder, result_path, speed=1)
    STDOUT.puts "creating animation from #{from_folder} into #{result_path}"
      Dir.mkdir images_folder if !Dir.exist? images_folder

      convert_to_images(from_folder, images_folder)

      `avconv -r #{speed} -qscale 1 -i #{images_folder}/%03d.gif -vcodec mjpeg -y #{result_path}`
  end
end


