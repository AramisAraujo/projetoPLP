#include <iostream>
#include <cstdlib>
#include <sstream>

using namespace std;

const int boardSize = 9;//Game Board size minimum 9 (9x9 game board)
const int bombs = int(((boardSize)*(boardSize))/10);//Bombs must be present in 10% of the game tiles
int remainingFlags = bombs;
int remainingTiles = (boardSize*boardSize) - bombs;

//Game Tiles
string coveredTile = "■";
string emptyTile = "□";
string flaggedTile = "🏴";
string bombTile = "💣";

int gameBoard[boardSize][boardSize];
string display[boardSize][boardSize];
int bombCoords[bombs][2];

enum gameOptions{
    OPEN_TILE = 1,
    FLAG_TILE,
    EXIT_GAME
};

enum tileStates{
    BOMB = -1,
    EMPTY_TILE
};

void printDisplay(){
  //Prints the display board
  for(int a = 0; a <= boardSize -1; a++){
      for(int b = 0; b <= boardSize -1; b++){
        if (b == 0) {
          cout << "               "<< a <<"   " << display[a][b] << "   ";
        }else {
          cout << display[a][b] << "   ";
        }
      }
      cout << endl<< endl;
  }
}


int showOptions(){
    int option;

    cout << "                   #Minesweeper#" << endl;
    cout << endl;
    cout << endl;
    cout << " Options" << endl;
    cout << " 1. To open a tile" << endl;
    cout << " 2. To flag/unflag a tile" << endl;
    cout << " 3. To end the game" << endl;
    cout << endl;
    cout << endl;
    cout << " - Remaining flags: " << remainingFlags + 1;
    cout << endl;
    cout << endl;
    cout << "                   0   "<< "1   " << "2   " << "3   "<< "4   "<< "5   "<< "6   "<< "7   "<< "8   "<< endl;
    cout << endl;
    
    printDisplay();
    
    //Asks user for interaction
    cout << endl;
    cout << "Type your option: ";
    cin >> option;
    cout << endl;
    return option;
}

int checkCoordinates(int x, int y){
  if(x < 0 || x > boardSize - 1){
      return 0;
  }
  else if(y < 0 || y > boardSize - 1){
      return 0;
  }
  return 1;
  
}

int gameOver() {
  cout << endl;
  cout << "Game Over!" << endl;
  cout << endl;
  cout << endl;

  return 0;
}

void askForCoordinates(int& xCoord, int& yCoord){
    cout <<"Please type Row and Column coordinates to open a tile." << endl;

    cin >> xCoord;//This is the Row (horizontal) coordinate
    cin >> yCoord;//This is the Column (vertical) coordinate

}

void getValidCoordinates(int& x, int& y){
    int coordinatesAreValid = 0;
    int attempt = 0;

    do{
        if(attempt > 0){
            cout << "Invalid Row or Column coordinates, please try again." << endl;
        }
        askForCoordinates(x, y);
        coordinatesAreValid = checkCoordinates(x, y);
        attempt++;

    }while(!coordinatesAreValid);
}

void setTileEmpty(int& x, int& y){
    display[y][x] = emptyTile;
}

void setTileFlagged(int& x, int& y){
    display[y][x] = flaggedTile;
}

int openEmptyTiles(int xCoord, int yCoord){

    if(!checkCoordinates(xCoord, yCoord)){//We check if the coordinates lead to a valid tile
        return 1;
    }
    if(display[yCoord][xCoord] != coveredTile){//We only want to keep going on covered tiles
      return 0;
    }
    else if(gameBoard[yCoord][xCoord] == -1){//We must stop on bombs (technically this should never be true)
        return 0;
    }

    else if(gameBoard[yCoord][xCoord] > 0){//If we found a number around a bomb, we must stop
        stringstream ss;

        ss << gameBoard[yCoord][xCoord];
        string str = ss.str();

        display[yCoord][xCoord] = str;

        remainingTiles--;
        return 0;
    }
    else{//We then proceed to the adjacent tiles
        setTileEmpty(xCoord, yCoord);
        remainingTiles--;

        if(yCoord > 0){ 
         openEmptyTiles(xCoord -1,yCoord -1);//Upper left corner
         openEmptyTiles(xCoord, yCoord -1);//Above
         openEmptyTiles(xCoord +1,yCoord -1);//Upper right corner
         }
        if(xCoord > 0){
          
        openEmptyTiles(xCoord -1,yCoord);//Left
        } 
        if(xCoord < boardSize -1){
          openEmptyTiles(xCoord +1,yCoord);//Right
        }
        if(yCoord < boardSize - 1){
         openEmptyTiles(xCoord -1,yCoord +1);//Lower left corner
         openEmptyTiles(xCoord, yCoord +1);//Below
         openEmptyTiles(xCoord +1,yCoord +1);//Lower right corner
        }
        
    }

}

int open_tile(){
    int xCoord;
    int yCoord;

    getValidCoordinates(xCoord, yCoord);

    if(gameBoard[yCoord][xCoord] == BOMB){
        //TODO: Fill Display with the 'answer'
        printDisplay();
        cout << "BOOOM! You have found a bomb ;)" << endl;
        return gameOver();
    }

    else if(display[yCoord][xCoord] == flaggedTile){
        cout <<"You cannot open flagged tiles!"<< endl;
        return 1;
    }

    else if(gameBoard[yCoord][xCoord] == EMPTY_TILE){
        openEmptyTiles(xCoord, yCoord);
        
    }

    else{
        stringstream ss;
        ss << gameBoard[yCoord][xCoord];
        string str = ss.str();
        display[yCoord][xCoord] = str;
        remainingTiles--;
    }

    if( remainingTiles == 0){
          return 0;
      }

    return 1;
}

void flag_tile(){
    int xCoord;
    int yCoord;
    getValidCoordinates(xCoord, yCoord);

    if(display[yCoord][xCoord] == flaggedTile){
        cout << "Removing flag from tile (" << xCoord <<", " << yCoord << ")" << endl;
        setTileEmpty(xCoord, yCoord);
        remainingFlags++;
    }

    else if (remainingFlags >= 0) {
        setTileFlagged(xCoord, yCoord);
        remainingFlags--;
    }else {
        cout << "You cannot flag any more tiles!"<< endl;
        cout << endl;
    }
}

int main(){

    int placedBombs = 0;
    srand(time(NULL));//We need to seed the randomizer

    while (placedBombs < bombs){//Generating and storing bomb coordinates
        int x = rand() % 8;
        int y = rand() % 8;

        if(gameBoard[y][x] == 0){
            gameBoard[y][x] = -1;//Places a bomb on the game board
            bombCoords[placedBombs][0] = y;//We then save the coordinates for that bomb
            bombCoords[placedBombs][1] = x;
            placedBombs++;
            //cout <<"Placed a bomb on x: " << x << " y: " << y << endl;//Spoilers

        }

    }

    for(int c = 0; c < bombs; c++){//Filling tiles around the bombs with tips

        int yCoord = bombCoords[c][0];//Get the coordinates for the bombs
        int xCoord = bombCoords[c][1];

        //TODO: make this mess into function calls
        if(yCoord > 0 && gameBoard[yCoord - 1][xCoord] > -1){//Add above bomb
            gameBoard[yCoord - 1][xCoord]++;//Adding to the tile adjacent to a bomb
        }

        if(yCoord > 0 && xCoord - 1 >= 0 && gameBoard[yCoord - 1][xCoord - 1] > -1){//Add upper left corner
            gameBoard[yCoord - 1][xCoord - 1]++;//Adding to the tile adjacent to a bomb
        }

        if(yCoord > 0 && xCoord + 1 < boardSize && gameBoard[yCoord - 1][xCoord + 1] > -1){//Add upper right corner
            gameBoard[yCoord - 1][xCoord + 1]++;//Adding to the tile adjacent to a bomb
        }

        if(yCoord < boardSize && gameBoard[yCoord + 1][xCoord] > -1){//Add below bomb
            gameBoard[yCoord + 1][xCoord]++;//Adding to the tile adjacent to a bomb

        }
        if(yCoord < boardSize && xCoord - 1 >= 0 && gameBoard[yCoord + 1][xCoord - 1] > -1){//Add lower left corner
            gameBoard[yCoord + 1][xCoord - 1]++;//Adding to the tile adjacent to a bomb
        }

        if(yCoord < boardSize && xCoord + 1 < boardSize && gameBoard[yCoord + 1][xCoord + 1] > -1){//Add lover right corner
            gameBoard[yCoord + 1][xCoord + 1]++;//Adding to the tile adjacent to a bomb
        }

        if(xCoord > 0 && gameBoard[yCoord][xCoord - 1] > -1){//Add to the left of the bomb
            gameBoard[yCoord][xCoord - 1]++;//Adding to the tile adjacent to a bomb
        }

        if(xCoord < boardSize && gameBoard[yCoord][xCoord + 1] > -1){//Add to the right of the bomb
            gameBoard[yCoord][xCoord + 1]++;//Adding to the tile adjacent to a bomb
        }
    }

    //cout << "Gameboard completed" << endl;

    for(int h = 0; h <= boardSize - 1; h++){//Fills the display as covered tiles
        for(int j = 0; j <= boardSize - 1; j++){
            display[h][j] = coveredTile;
        }
    }

    //cout << "Display Ready" << endl;

//Game Execution Loop
    int gameEnded = 1;

    while (gameEnded != 0){

      int option = 0;
      
      cout << "Remaining Tiles: " << remainingTiles << endl;

      option = showOptions();

      if(option == OPEN_TILE){
          gameEnded = open_tile();
      }

      else if(option == FLAG_TILE){
          flag_tile();
      }

      else if(option == EXIT_GAME){
          gameEnded = gameOver();
      }

      else {
          cout<< "Invalid option, please insert a valid option to proceed." << endl;
      }
    }

    if(remainingTiles == 0){

        cout <<"Congratulations, you won!"<< endl;
    }

    return(0);
}
