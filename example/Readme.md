The directory for building this docker example needs to be the root of the repository (as it need access to the sources).

```
$> docker build -f Dockerfile -t build_example ..
```
After building the container with the build result can be accessed:
```
$> docker run -it build_example
#container> cd /example/example
#container> ./exampe #runs the builded executable
```
To delete the image afterwards simply call
```
$> docker rmi build_example
```
