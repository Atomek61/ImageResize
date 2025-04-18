        ��  ��                  �      �� ��               �4   V S _ V E R S I O N _ I N F O     ���                 ?                        T   S t r i n g F i l e I n f o   0   0 4 0 9 0 4 E 4   `   F i l e D e s c r i p t i o n     J P E G   a n d   P N G   i m a g e   s c a l i n g     0   I n t e r n a l N a m e   i m g r e s     t (  L e g a l C o p y r i g h t   �   2 0 2 4   J a n   S c h i r r m a c h e r ,   w w w . a t o m e k . d e        C o m m e n t s       $   C o m p a n y N a m e         0   F i l e V e r s i o n     4 . 1 . 0 . 0   ,   L e g a l T r a d e m a r k s         ,   O r i g i n a l F i l e n a m e       $   P r o d u c t N a m e         (   P r o d u c t V e r s i o n       D    V a r F i l e I n f o     $    T r a n s l a t i o n     	��  ,   ��
 C L I H E L P                 Free tool for JPEG and PNG quality scaling.

  Usage: imgres source target size {-option [value]}

  source              Is one of the following:
                      - a single JPEG or PNG file
                      - the name of a text file preceeded by @ with a list of filenames
                      - a path to a folder with semicolon-separated wildcards
  target              The folder where to store the resulting resampled and resized files.
                      If the resulting filenames can conflict and this is not solved by renaming
                      then the folder must contain the placeholder ${SIZE} or ${SIZENAME}.
                      Non-existend folders are created.
  size                Is a size in pixels or a comma-separated list of sizes, where the size refers to
                      the longer side of the image.
  {-option [value]}   Is a list of pairs of option/value. Some options have no value.

Options:

short long            parameter comment
-----------------------------------------
   -j -JPEG           1..100    A quality from 1 to 100 percent (default is 75)
   -p -PNG            compr     Compression: Default|None|Fastest|Maximum
   -i -interpolation  name      Stretch, Box, Linear, HalfCosine, Cosine, Bicubic, Mitchell, Spline,
                                Lanczos2, Lanczos3, Lanczos4, BestQuality
                                Default is BestQuality: Mitchell on up- and Spline on down-scaling
   -r -rename         template  Rename files by a template with placeholders.
                                ${SIZE} and ${INDEX.ifmt(n,3)} with n=startindex, d=number of digits/auto
   -n -sizenames      names     If using the ${SIZENAME} placeholder in file renaming template, or in
                                in the target folders name, then each size must have a corresponding
                                list of comma-separated names
   -w -watermark      file.png  A PNG watermark file and optional size, position and opacity, see example
   -s -shuffle        [0..n]    Mixes the image list, makes sense together with -r.
                                An optional random seed of 0 (assumed if ommited) means an unpredictable
                                sequence. A fixed value will always shuffle a list in the same way if the
                                number of files has not changed.
   -z -sharpen        0..100..  Sharpen, a value >= 0: 0 off, 50 light, 100 good, 200 strong
   -m -meta           sources   EXIF | TAGS | EXIF,TAGS
                                EXIF loads tags from origin files, TAGS from .tags files
   -e -exif           taglist   Writes certain metadata into the resized files.
                                Taglist is a list of special tagnames: "Title,Timestamp,Copyright"
                                if no meta sources is defined, EXIF is assumed.
   -c -copyright      "text"    Writes (overrides) the EXIF or .tags copyright tag.
   -l -listing        listings  Exports infos - listings is a comma-separated combination of "tagsreport"
                                and "imgtags", e.g. "imgtags" or "tagsreports,imgtags".
   -t -threads        0..n      Number of threads to use, 0 means maximum
   -x -stoponerror              Stop all tasks if an error occures
   -h -help                     Outputs this text
   -d -dryrun                   Dry run, does not create images, but if applicable the listings
   -q -quiet                    Suppresses any message output

Examples:

  imgres myimage.png \Images\res640 640
    resamples a single png image with the default quality.

  imgres ..\theimage.jpg C:\TEMP\res${SIZE} 480,640,800 -j 50
    resamples a single jpg with 3 resolutions and stores them to different folders at 50% quality

  imgres @mylist.txt img${SIZE} 640,1920 -j 1 -p maximum -q
    resamples a list of files which path/name is stored in a file, with smallest file sizes in quiet mode.

  imgres myimages\*.jpg;*.png \MyImages 640 -w mywatermark.png:10,1,98:20
    adds a watermark of 10% width, 1% from the left, 2% from the bottom, with 20% opacity.

  imgres DSC4205.jpg C:\TEMP 640 -c "(c) 1941 ACME"
    inserts an EXIF copyright note

  More info at www.atomek.de/imageresize/hlp40/gui/en/index.htm
