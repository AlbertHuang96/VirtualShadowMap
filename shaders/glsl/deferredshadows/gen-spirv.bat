REM glslangvalidator -V shadow.geom -o shadow.geom.spv
glslangvalidator -V deferred.vert -o deferred.vert.spv
glslangvalidator -V deferred.frag -o deferred.frag.spv
glslangvalidator -V markUsedVirtualTiles.comp -o markUsedVirtualTiles.comp.spv
glslangvalidator -V preparePhysicalTiles.comp -o preparePhysicalTiles.comp.spv

REM glslangvalidator -V deferred.frag -o deferred.frag.spv
REM glslangvalidator -V mrt.frag -o mrt.frag.spv

glslangvalidator -V shadow.vert -o shadow.vert.spv
glslangvalidator -V shadow.frag -o shadow.frag.spv

glslangvalidator -V mrt.frag -o mrt.frag.spv

glslangvalidator -V frustumCulling.comp -o frustumCulling.comp.spv

pause