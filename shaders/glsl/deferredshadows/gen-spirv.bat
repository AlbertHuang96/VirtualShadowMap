REM glslangvalidator -V shadow.geom -o shadow.geom.spv
REM glslangvalidator -V deferred.frag -o deferred.frag.spv
glslangvalidator -V markUsedVirtualTiles.comp -o markUsedVirtualTiles.comp.spv
glslangvalidator -V preparePhysicalTiles.comp -o preparePhysicalTiles.comp.spv

pause