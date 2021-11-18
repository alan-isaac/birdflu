"""
We are going to take the original intersection data
and scale up the world to match the NetLogo model extent.
Note: don't change the road data,
which connects intersections based on their integer ids.
Comment: larger scale factor will exponentially slow setup.
"""
scale = 2  #scale factor
with open("original_intersections.txt",'r') as fin, open("intersections.txt",'w') as fout:
	result = [next(fin).strip()]
	for line in fin:
		x, y, name, itype = line.split()
		#separate by *single* space
		result.append("{} {} {} {}".format(scale*int(x), scale*int(y), name, itype))
	fout.write("\n".join(result))

