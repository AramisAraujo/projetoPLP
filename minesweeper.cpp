#include <iostream>
#include <cstdlib>
#include <sstream>
#include <time.h>

using namespace std;

const int boardSize = 9;//Game Board size minimum 9 (9x9 game board)
const int bombs = int(((boardSize)*(boardSize))/10);//Bombs must be present in 10% of the game tiles
int remainingFlags = bombs;

//Game Tiles
string coveredTile = "■";
string emptyTile = "□";
string flaggedTile = "◈";

int gameBoard[boardSize][boardSize];
string display[boardSize][boardSize];

int showOptions(){
    int option;

    cout << "                   #Minesweeper#" << endl;
    cout << endl;
    cout << endl;
    cout << " Options" << endl;
    cout << " 1. To open a tile" << endl;
    cout << " 2. To flag a tile" << endl;
    cout << " 3. End game" << endl;
    cout << endl;
    cout << endl;
    cout << " - Remaining flags: " << remainingFlags + 1;
    cout << endl;
    cout << endl;
    cout << "                   0   "<< "1   " << "2   " << "3   "<< "4   "<< "5   "<< "6   "<< "7   "<< "8   "<< endl;
    cout << endl;

    //Prints the display board
    for(int a = 0; a <= boardSize -1; a++){
        for(int b = 0; b <= boardSize -1; b++){
          if (b == 0) {
            cout << "               "<< a <<"   " << display[a][b] << "   ";
          }else {
            cout << display[a][b] << "   ";
          }
        }
        cout << endl;
        cout << endl;
    }

    //Asks user for interaction
    cout << endl;
    cout << "Type your option: ";
    cin >> option;
    //Don't forget to catch empty spaces/line terminators
    cout << endl;
  return option;
}

int checkCoordinates(int x, int y){
  if(x < 0 || x > boardSize - 1){
      cout << "Invalid column coordinate, please try again." << endl;
      cout << endl;
  }
  else if(y < 0 || y > boardSize - 1){
      cout << "Invalid row coordinate, please try again." << endl;
      cout << endl;
  }
  else{
      return 1;
  }
  return 0;
}

int gameOver() {
  cout << endl;
  cout << "Game Over!" << endl;
  cout << endl;
  cout << endl;
  return 0;
}

void ask_by_the_coordinate(int& xCoord, int& yCoord){
    cout <<"Please type X and Y coordinates to open a tile." << endl;

            //Don't forget to catch empty spaces/line terminators
    cin >> xCoord;//This is the horizontal coordinate
    cin >> yCoord;//This is the vertical coordinate

}

void add_valid_coordinates(int& x, int& y){
    int coordinatesAreValid = 0;

    while(!coordinatesAreValid){
            ask_by_the_coordinate (x, y);
            coordinatesAreValid = checkCoordinates(x, y);
    }
}

void empty_a_tile(int& x, int& y)
{
    display[y][x] = emptyTile;
}

void flag_a_tile(int& x, int& y)
{
    display[y][x] = flaggedTile;
}

enum open_tile_reaction
{
    ITS_A_BOMB = -1,
    EMPTY_TILE
};

int open_tile()
{
    int xCoord;
    int yCoord;
    add_valid_coordinates(xCoord, yCoord);
            //TODO: if coordinates match with a bomb end game, else open tile or empty spaces

    cout <<"x: " << xCoord << " y: " << yCoord << endl;//Spoilers
    cout <<"bomb : " << gameBoard[xCoord][yCoord] << endl;//Spoilers



    if(gameBoard[xCoord][yCoord] == ITS_A_BOMB)
    {
        return gameOver();
    }else if(gameBoard[xCoord][yCoord] == EMPTY_TILE)
    {
        empty_a_tile(xCoord, yCoord);
    }else
    {
        stringstream ss;
        ss << gameBoard[yCoord][xCoord];
        string str = ss.str();
        display[yCoord][xCoord] = str;
    }
            return 1;
}

void flag_tile()
{
    int xCoord;
    int yCoord;
    add_valid_coordinates(xCoord, yCoord);

    if (remainingFlags >= 0) {
        flag_a_tile(xCoord, yCoord);
        remainingFlags--;
    }else {
        cout << "You cannot to flag anymore!"<< endl;
        cout << endl;
    }
}

enum game_options
{
    OPEN_TILE = 1,
    FLAG_TILE,
    EXIT_GAME
};


int main(){

    int placedBombs = 0;
    int bombCoord[bombs][2];
    srand(time(NULL));//We need to seed the randomizer

    while (placedBombs < bombs){
        int x = rand() % 8;
        int y = rand() % 8;

        if(gameBoard[y][x] == 0){
            gameBoard[y][x] = -1;//Places a bomb on the game board
            bombCoord[placedBombs][0] = y;//We then save the coordinates for that bomb
            bombCoord[placedBombs][1] = x;
            placedBombs++;
            cout <<"Placed a bomb on x: " << x << " y: " << y << endl;//Spoilers

        }

    }

    for(int c = 0; c < bombs; c++){

        int yCoord = bombCoord[c][0];
        int xCoord = bombCoord[c][1];

        if(yCoord > 0 && gameBoard[yCoord - 1][xCoord] > -1){//add above bomb
            gameBoard[yCoord - 1][xCoord]++;//Adding to the tile adjacent to a bomb
        }

        if(yCoord > 0 && xCoord - 1 >= 0 && gameBoard[yCoord - 1][xCoord - 1] > -1){//add upper left corner
            gameBoard[yCoord - 1][xCoord - 1]++;//Adding to the tile adjacent to a bomb
        }

        if(yCoord > 0 && xCoord + 1 < boardSize && gameBoard[yCoord - 1][xCoord + 1] > -1){//add upper right corner
            gameBoard[yCoord - 1][xCoord + 1]++;//Adding to the tile adjacent to a bomb
        }

        if(yCoord < boardSize && gameBoard[yCoord + 1][xCoord] > -1){//add below bomb
            gameBoard[yCoord + 1][xCoord]++;//Adding to the tile adjacent to a bomb

        }
        if(yCoord < boardSize && xCoord - 1 >= 0 && gameBoard[yCoord + 1][xCoord - 1] > -1){//add lower left corner
            gameBoard[yCoord + 1][xCoord - 1]++;//Adding to the tile adjacent to a bomb
        }

        if(yCoord < boardSize && xCoord + 1 < boardSize && gameBoard[yCoord + 1][xCoord + 1] > -1){//add lover right corner
            gameBoard[yCoord + 1][xCoord + 1]++;//Adding to the tile adjacent to a bomb
        }

        if(xCoord > 0 && gameBoard[yCoord][xCoord - 1] > -1){//add to the left of the bomb
            gameBoard[yCoord][xCoord - 1]++;//Adding to the tile adjacent to a bomb
        }

        if(xCoord < boardSize && gameBoard[yCoord][xCoord + 1] > -1){//add to the right of the bomb
            gameBoard[yCoord][xCoord + 1]++;//Adding to the tile adjacent to a bomb
        }
    }

    //cout << "Gameboard completed" << endl;

    for(int h = 0; h <= boardSize - 1; h++){//The display starts out as covered tiles
        for(int j = 0; j <= boardSize - 1; j++){
            display[h][j] = coveredTile;
        }
    }

    //cout << "Display Ready" << endl;

    int gameEnded = 1;

    while (gameEnded != 0){

      int option = 0;

      option = showOptions();

      if(option == OPEN_TILE)
      {
          gameEnded = open_tile();
      }else if(option == FLAG_TILE)
      {
          flag_tile();
      }else if(option == EXIT_GAME)
      {
          gameEnded = gameOver();
      }
    }

    return(0);
}


