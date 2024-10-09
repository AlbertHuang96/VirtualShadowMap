REM glslangvalidator -V shadow.geom -o shadow.geom.spv
REM glslangvalidator -V deferred.frag -o deferred.frag.spv
REM glslangvalidator -V markUsedVirtualTiles.comp -o markUsedVirtualTiles.comp.spv
REM glslangvalidator -V preparePhysicalTiles.comp -o preparePhysicalTiles.comp.spv

REM glslangvalidator -V deferred.frag -o deferred.frag.spv
REM glslangvalidator -V mrt.frag -o mrt.frag.spv

glslangvalidator -V scene.vert -o scene.vert.spv
glslangvalidator -V scene.frag -o scene.frag.spv

pause