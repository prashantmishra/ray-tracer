#ScalaRayTracer

Hello there!

This is a ray-tracer written in Scala. It is capable of generating a scene with spheres, square-tiled planes and multiple light sources. It also has reflections, refractions and anti-aliasing through supersampling.

##Compiling and running

- Go inside the project directory : cd ${PROJECT_DIR}
- Sbt compile : ```sbt compile```
- Sbt run : ```sbt run```

This will display the scene corresponding to the file "samples/sample.txt"

##Example runs

Generated scene for "samples/sample1.txt":

![img1](https://raw.githubusercontent.com/prashantmishra/ScalaRayTracer/master/samples/sample1.png)

Generated scene for "samples/sample2.txt":

![img2](https://raw.githubusercontent.com/prashantmishra/ScalaRayTracer/master/samples/sample2.png)

##TO-DO

- Calculate the pixels in parallel.
- Use json to deserialize objects.
