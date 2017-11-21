### Compile on deepgreen with:
`nvcc ./start.cu 'pkg-config --cflags --libs opencv' -o main.out`

### Start with: 
`./main.out testimages/dice.png testimages/output.png`

### TODO
- Benchmark everything
- timer for non-cuda stuff
- Test all on deepgreen
- investigate dice.png issue 
- investigate segmentation fault on deepgreen


