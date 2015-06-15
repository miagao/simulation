public class Percolation {


    private    int N;
    private    WeightedQuickUnionUF grid, fullgrid;
    private    boolean[][] open;
    private    int virtualnodestart, virtualnodeend;

    public Percolation(int N) {
        // create N-by-N grid, with all sites blocked 
        //-- all grids are closed on creation time
        if (N <= 0  ) 
            throw new IllegalArgumentException("N must be positive");
        this.N = N;
        this.fullgrid = new WeightedQuickUnionUF(N*N + 2);
        this.grid = new WeightedQuickUnionUF(N*N + 1);
        this.open = new boolean[N][N];
        this.virtualnodestart = N*N;
        this.virtualnodeend = N*N + 1;
        
        for (int i = 0; i < N; i++) {
            for (int j = 0; j < N; j++) {
                open[i][j] = false;
            }
            //every row is connected to virtual node N*N and N*N+1 
            fullgrid.union(virtualnodestart, xyTo1D(1, i+1));
            grid.union(virtualnodestart, xyTo1D(1, i+1));
            fullgrid.union(virtualnodeend, xyTo1D(N, i+1));
        }
    }

    // open site (row i, column j) if it is not already
    public void open(int i, int j) {
        testBounds(i, j);
        open[i-1][j-1] = true;

        // Connect all sites around i,j  if they are opened unless its a corner
        int p = (i - 1);
        if (p >= 1 && isOpen(p, j)) { 
            fullgrid.union(xyTo1D(i, j), xyTo1D(p, j));
            grid.union(xyTo1D(i, j), xyTo1D(p, j));
        }
        p = i + 1;
        if (p <= N && isOpen(p, j)) { 
            fullgrid.union(xyTo1D(i, j), xyTo1D(p, j));
            grid.union(xyTo1D(i, j), xyTo1D(p, j));
        }
        int q = j - 1;
        if (q >= 1 && isOpen(i, q)) { 
            fullgrid.union(xyTo1D(i, j), xyTo1D(i, q));
            grid.union(xyTo1D(i, j), xyTo1D(i, q));
        }
        q = j + 1;
        if (q <= N && isOpen(i, q)) { 
            fullgrid.union(xyTo1D(i, j), xyTo1D(i, q));
            grid.union(xyTo1D(i, j), xyTo1D(i, q));
        }
        
    }


    // is site (row i, column j) open?
    public boolean isOpen(int i, int j) { 
        testBounds(i, j);
        return open[i-1][j-1];
    }

    public boolean isFull(int i, int j) { 
        testBounds(i, j);
        return (isOpen(i, j) && grid.connected(virtualnodestart, xyTo1D(i, j)));
    }

    public boolean percolates() {
        if (N != 1) { 
            return fullgrid.connected(virtualnodestart, virtualnodeend);
        } else {
            return isOpen(1, 1);
        }
    }

    private boolean testBounds(int i, int j) { 
        if (i <= 0 || i > N || j <= 0 || j > N)
            throw new IndexOutOfBoundsException("row index out of bounds");
        return true;
    }

    /*
     *method to translate 2D models for 1D to use Union-Find
     */
    private int xyTo1D(int x, int y) {
        return (y-1)*N + (x-1);
    }

    public static void main(String[] args) {
    }

}