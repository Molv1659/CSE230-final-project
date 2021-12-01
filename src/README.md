## UI features 
### Run the code
To run the current version, in your terminal under the root directory of this repo, run
```bash
cabal run
```
which should build and run the program in the current terminal. Currently, the UI should look something like this:
![image](pic/EmptyStaticBoard.png)
### drawing features
- [x] create seamless division of UI
- [x] use border to draw go board
- [ ] draw column/row number of the board
- [ ] find out how to draw go stone
- [ ] draw stone dynamically according to the stone state in the logic part

### events
- [x] redraw upon event, quit with events other then resize.
- [ ] Get mouse position and translate them into the location of stones.