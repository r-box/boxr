# Conventions

boxr uses the following naming conventions:

* `snake_case` names are used for exported functions.
* `snake_case` names are also used for *internal* functions, which an 
     advanced user might stumble across and want to use. They are not exported
     because they're probably not terribly useful to most people.
* `camelCase` names are used for internal functions which probably won't play
    well out of thier intended environments, or may have output which is 
    difficult to interpret. :dragon_face: Here be Dragons :fire:
    