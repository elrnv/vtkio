import pygmsh

with pygmsh.geo.Geometry() as geom:
    p = geom.add_polygon(
        [
            [0.0, 0.0],
            [1.0, -0.2],
            [1.1, 1.2],
            [0.1, 0.7],
        ],
        mesh_size=0.4,
    )
    geom.add_physical(p.lines[0], label="bottom")
    geom.add_physical(p.lines[1], label="right")
    geom.add_physical(p.lines[2], label="top")
    geom.add_physical(p.lines[3], label="left")

    mesh = geom.generate_mesh()

mesh.write("no-compression.vtu", compression=None)
mesh.write("lzma.vtu", compression="lzma")
mesh.write("zlib.vtu", compression="zlib")
mesh.write("ascii.vtu", binary=False)
mesh.write("binary.vtk")
mesh.write("ascii.vtk", binary=False)
