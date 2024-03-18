
######## actual game components
#MAIN FUNCTION
main_fun <- function(){
    exit <- FALSE
    while(exit == FALSE){
    #runs the game
    hide_n_seek()
    }
    
}

hide_n_seek <-function(){
    game_over < - FALSE
    while(game_over == FALSE){
    p1 <- create_player()
    #prompt user to enter their name
    p1[1,2] <- user_said()
    #set user to seeking to start the game on their turn
    p1[4,2] <-"Seeking"
    #user will pick a hiding place
    set_hiding_place(p1)
    #cpu setup
    cpu <-create_player()
    set_hiding_place(cpu)
    move_finder(p1,cpu)
    }
    exit <- TRUE
    return (exit)


}

##get user input
##user input function
user_said <- function() {
    response <- readline(prompt="Enter your response... ")
    return (response)
}

create_player <- function() {
    player <- matrix (c("Name: ", "Location X: ", "Location Y: ", "Status: ","Guesses: ","CPU",0,0, "Hiding",c()), nrow =5, ncol=2)
    return (player)
}

move_finder <- function (person1, person2) {
    
    if(person2[4,2] == "Seeking"){
        #person[4,2] <- "finding"
        print(paste("Watch out!!! ", person1[1,2]," is looking for you!!!"))
        rando_x <- sample(0:10, 1)
        rando_y <- sample(0:10, 1)
        guess < - c(rando_x,rando_y)
        guesses <- person2[5,2]
        if(length(guesses) > 0){
            #pulling an already done guess from the players guess bank
            for(g in guesses){
                #if the guess is the same as the current guess the player is trying to make then they lose their turn
                if(g == guess){
                    print("you already guessed this spot!!!\n\nYou lose your turn")
                    person2[4,2] <- "Hiding"
                    person1[4,1] <- "Seeking"
                    move_finder(person1,person2)
                }else{
                    # if this is a brand new guess, we will add it to the player's guess bank
                    person1[5,2] <- c(guesses,guess)
                    # send the guess to check if the cpu is there.
                    check_positions(person1,person2,guess)
                }
            }
        }
        



    }else{
        plot_guesses(person1)
        print("the player is finding\nWhere do you want to check?\nEnter a number for the 'X' axis.")
        x_check <- as.numeric(user_said())
        print("Enter for the 'Y' axis")
        y_check <- as.numeric(user_said())
        guess <- c(x_check,y_check)
        guesses <- person1[5,2]
        #if the player has at least made 1 guess we need to check it
        if(length(guesses) > 0){
            #pulling an already done guess from the players guess bank
            for(g in guesses){
                #if the guess is the same as the current guess the player is trying to make then they lose their turn
                if(g == guess){
                    print("you already guessed this spot!!!")
                }else{
                    # if this is a brand new guess, we will add it to the player's guess bank
                    person1[5,2] <- c(guesses,guess)
                    # send the guess to check if the cpu is there.
                    check_positions(person1,person2,guess)
                }
            }
        }

        #person[5,2] <- c(x_check,y_check)
        






    }
    #print(person)
}#end of move_finder

#check_positions function
check_positions <- function( person1, person2, guess){
    #if the user made the guess
    if(person1[4,2]=="Seeking"){
        if(person2[2,2] == guess [1,1] && person2[3,2] == guess [1,2]){
            person2[4,2] <- "FOUND!!!"
            print("Congrats you found the CPU and won the game!!!")
            game_over <- TRUE
            return(game_over)
        }else{
            print("The game continues...\n\n")
            #plot the points
            person1[4,2] <- "Hiding"
            person2[4,2] <- "Seeking"
            move_finder(person1,person2)
        }
    }else{
        if(person1[2,2] == guess [1,1] && person1[3,2] == guess [1,2]){
            person1[4,2] <- "FOUND!!!"
            print("OH NO YOU WERE FOUND!!!\n\nYou Lose...\n\n")
            game_over <- TRUE
            return(game_over)
        }else{
            print("You got lucky that the CPU didn't find you...\n\nIt's your turn to go find the CPU\n\n")
            #plot the points
            person2[4,2] <- "Hiding"
            person1[4,2] <- "Seeking"
            move_finder(person1,person2)
        }
    }

} # end of check_positions


#plot guesses 
plot_guesses <- function(person) {
    guesses <- person[5,2]
    for (g in guesses){
        if(person[1,2] != "CPU"){
            pcolor <- "red"
            
        }else{
            pcolor <- "green"
        }
        plot_name <- paste(person[1,2], "Guessing Plot")
        x_plot <- g[1,1]
        y-plot <- g[1,2]
        plot(x_plot,y_plot, pch=10, cex=2, col = pcolor, main = plot_name )
        
    }
}#end of plot guesses 

set_hiding_place <- function(person){
    if(person[1,2] == "CPU"){
        #print("The CPU has picked it's hiding spot which is at (?,?) \nHere's a hint, it between (0,0) and (10,10)")
        rando_x <- sample(0:10, 1)
        rando_y <- sample(0:10, 1)
        person[2,2] <- rando_x
        person[3,2] <- rando_y
        print(person) #debugging line
    }else{
        print("The CPU has picked it's hiding spot which is at (?,?) \nHere's a hint, it between but including (0,0) and (10,10)")
        print("Pick an x corridnate where you want to hide. No more than 10, no less than 0, whole numbers only\n\n")
        x_plot <- as.numeric(user_said())
        print("Pick an y corridnate where you want to hide. No more than 10, no less than 0, whole numbers only\n\n")
        y_plot <- as.numeric(user_said())
        person[2,2] <- x_plot
        person[3,2] <- y_plot
        if((x_plot > 10 || x_plot < 0) && (y_plot > 10 || y_plot < 0)){
            print("invalid entry!!! You need to pick no less than 0 and no greater than 10 on either axis")
            set_hiding_place(person)
        }
        print("you have selected to hide here --->(", person[2,2],",",person[3,2],")\n\n")
    }
}#end of set_hiding_place





#

create_player <- function() {
    player <- data.frame (
        player_name = "CPU",
        boats = 3,
        status = "Hiding"
    )
}

##scoreboard data frame
scoreboard <- data.frame (
    player = c(user, cpu),
    boats_left =   c(3,3)
    
)

cpu <- create_player()
print(cpu)




######## actual game components
#MAIN FUNCTION
main_fun <- function(){
    exit <- FALSE
    while(exit == FALSE){
    #runs the game
    hide_n_seek()
    }
    
}

hide_n_seek <-function(){
    game_over < - FALSE
    while(game_over == FALSE){
    p1 <- create_player()
    #prompt user to enter their name
    p1[1,2] <- user_said()
    #set user to seeking to start the game on their turn
    p1[4,2] <-"Seeking"
    #user will pick a hiding place
    set_hiding_place(p1)
    #cpu setup
    cpu <-create_player()
    set_hiding_place(cpu)
    move_finder(p1,cpu)
    }
    exit <- TRUE
    return (exit)


}

##get user input
##user input function
user_said <- function() {
    response <- readline(prompt="Enter your response... ")
    return (response)
}

create_player <- function() {
    player <- matrix (c("Name: ", "Location X: ", "Location Y: ", "Status: ","Guesses: ","CPU",0,0, "Hiding",c()), nrow =5, ncol=2)
    return (player)
}

move_finder <- function (person1, person2) {
    
    if(person2[4,2] == "Seeking"){
        #person[4,2] <- "finding"
        print(paste("Watch out!!! ", person1[1,2]," is looking for you!!!"))
        rando_x <- sample(0:10, 1)
        rando_y <- sample(0:10, 1)
        guess < - c(rando_x,rando_y)
        guesses <- person2[5,2]
        if(length(guesses) > 0){
            #pulling an already done guess from the players guess bank
            for(g in guesses){
                #if the guess is the same as the current guess the player is trying to make then they lose their turn
                if(g == guess){
                    print("you already guessed this spot!!!\n\nYou lose your turn")
                    person2[4,2] <- "Hiding"
                    person1[4,1] <- "Seeking"
                    move_finder(person1,person2)
                }else{
                    # if this is a brand new guess, we will add it to the player's guess bank
                    person1[5,2] <- c(guesses,guess)
                    # send the guess to check if the cpu is there.
                    check_positions(person1,person2,guess)
                }
            }
        }
        



    }else{
        plot_guesses(person1)
        print("the player is finding\nWhere do you want to check?\nEnter a number for the 'X' axis.")
        x_check <- as.numeric(user_said())
        print("Enter for the 'Y' axis")
        y_check <- as.numeric(user_said())
        guess <- c(x_check,y_check)
        guesses <- person1[5,2]
        #if the player has at least made 1 guess we need to check it
        if(length(guesses) > 0){
            #pulling an already done guess from the players guess bank
            for(g in guesses){
                #if the guess is the same as the current guess the player is trying to make then they lose their turn
                if(g == guess){
                    print("you already guessed this spot!!!")
                }else{
                    # if this is a brand new guess, we will add it to the player's guess bank
                    person1[5,2] <- c(guesses,guess)
                    # send the guess to check if the cpu is there.
                    check_positions(person1,person2,guess)
                }
            }
        }

        #person[5,2] <- c(x_check,y_check)
        






    }
    #print(person)
}#end of move_finder

#check_positions function
check_positions <- function( person1, person2, guess){
    #if the user made the guess
    if(person1[4,2]=="Seeking"){
        if(person2[2,2] == guess [1,1] && person2[3,2] == guess [1,2]){
            person2[4,2] <- "FOUND!!!"
            print("Congrats you found the CPU and won the game!!!")
            game_over <- TRUE
            return(game_over)
        }else{
            print("The game continues...\n\n")
            #plot the points
            person1[4,2] <- "Hiding"
            person2[4,2] <- "Seeking"
            move_finder(person1,person2)
        }
    }else{
        if(person1[2,2] == guess [1,1] && person1[3,2] == guess [1,2]){
            person1[4,2] <- "FOUND!!!"
            print("OH NO YOU WERE FOUND!!!\n\nYou Lose...\n\n")
            game_over <- TRUE
            return(game_over)
        }else{
            print("You got lucky that the CPU didn't find you...\n\nIt's your turn to go find the CPU\n\n")
            #plot the points
            person2[4,2] <- "Hiding"
            person1[4,2] <- "Seeking"
            move_finder(person1,person2)
        }
    }

} # end of check_positions


#plot guesses 
plot_guesses <- function(person) {
    guesses <- person[5,2]
    for (g in guesses){
        if(person[1,2] != "CPU"){
            pcolor <- "red"
            
        }else{
            pcolor <- "green"
        }
        plot_name <- paste(person[1,2], "Guessing Plot")
        x_plot <- g[1,1]
        y-plot <- g[1,2]
        plot(x_plot,y_plot, pch=10, cex=2, col = pcolor, main = plot_name )
        
    }
}#end of plot guesses 

set_hiding_place <- function(person){
    if(person[1,2] == "CPU"){
        #print("The CPU has picked it's hiding spot which is at (?,?) \nHere's a hint, it between (0,0) and (10,10)")
        rando_x <- sample(0:10, 1)
        rando_y <- sample(0:10, 1)
        person[2,2] <- rando_x
        person[3,2] <- rando_y
        print(person) #debugging line
    }else{
        print("The CPU has picked it's hiding spot which is at (?,?) \nHere's a hint, it between but including (0,0) and (10,10)")
        print("Pick an x corridnate where you want to hide. No more than 10, no less than 0, whole numbers only\n\n")
        x_plot <- as.numeric(user_said())
        print("Pick an y corridnate where you want to hide. No more than 10, no less than 0, whole numbers only\n\n")
        y_plot <- as.numeric(user_said())
        person[2,2] <- x_plot
        person[3,2] <- y_plot
        if((x_plot > 10 || x_plot < 0) && (y_plot > 10 || y_plot < 0)){
            print("invalid entry!!! You need to pick no less than 0 and no greater than 10 on either axis")
            set_hiding_place(person)
        }
        print("you have selected to hide here --->(", person[2,2],",",person[3,2],")\n\n")
    }
}#end of set_hiding_place
