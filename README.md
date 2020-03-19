# HaskellGym

## Description
This is a simple gym app written in Haskell that will tell you what exercises you should be doing based on the block and week you are in and historical exercise data, where there are 4 blocks and 4 weeks in a block.
I built it to gain some real-world Haskell experience in the form of building an application with a GUI and data persistence. 

## Usage
- IMPORTANT: Ensure the terminal window is close to full screen to ensure all data is displayed. 
- The app is build using cabal, so use cabal build to build it and cabal run in the terminal to run it
- When you run the app, you will see a form to input your name, the block and the week. After inputting your data, press enter to see your workout list. To exit the program, press [esc].
- As of now, there is no mechanism to input historical data through the GUI, so you have to modify your local csv file. A .csv file is included in this repo that can be modified. It includes sample data.
  Note: do not change the name of the .csv file, as it is hardcoded into the program at the moment.
  
## Libraries Used
- Brick for GUI - https://hackage.haskell.org/package/brick
- Cassava for data persistence - https://hackage.haskell.org/package/cassava
- Lens for some data access - https://hackage.haskell.org/package/lens
