(annotation? '<html>)

(annotation? '<p>)

(end-annotation '<html>)

(write-file
 (list '<p> 'hello 'world 'is 'the 'most 'stupid 'program 'in 'the 'world '</p>))