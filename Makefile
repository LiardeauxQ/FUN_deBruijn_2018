##
## EPITECH PROJECT, 2018
## Makefile
## File description:
## Makefile
##

NAME	=	deBruijn

BIN_PATH	=	$(shell stack path --local-install-root)

all:    $(NAME)

$(NAME):
	@printf "Building deBruijn Project\n"
	@stack build
	@printf "Create deBruijn binarie $(NAME)\n"
	@cp $(BIN_PATH)/bin/deBruijn-exe $(NAME)

clean:
	@rm $(NAME)
	@printf "Clean: OK\n"

re: fclean all

.PHONY: re fclean all
