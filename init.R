###################################################################
#                                                                 #
# Author: B. Philipp Kleer                                        #
# Credits: https://github.com/ebeneditos/telegram.bot             #
#                                                                 #
###################################################################

# For use of this, see README.MD

# > Loading libraries ----
library("telegram.bot")
library("tidyverse")
setwd(paste0(getwd(), "/telegramBot"))

# > Custom keyboard replies ----
# with these lists we adjust keyboard responses in telegram
keyStart <- ReplyKeyboardMarkup(
  keyboard = list(
    list(KeyboardButton("\xF0\x9F\x95\x90 Wann?")),
    list(KeyboardButton("\xF0\x9F\x91\xB7 Vorleistung")),
    list(KeyboardButton("\xF0\x9F\x93\x9A Prüfung")),
    list(KeyboardButton("\xF0\x9F\x93\x85 Sprechstunde")),
  ),
  resize_keyboard = FALSE,
  one_time_keyboard = TRUE
)

keyExams <- ReplyKeyboardMarkup(
  keyboard = list(
    list(
      KeyboardButton("\xF0\x9F\x9A\xA8 Anmeldung"),
      KeyboardButton("\xF0\x9F\x98\xB7 Attest/Krankheit"),
      KeyboardButton("\xE2\x9D\x93 Note"),
      ),
    list(
      KeyboardButton("\xE2\x9D\x8C Durchgefallen?"),
      KeyboardButton("/start \xF0\x9F\x94\x83")
      )
  )
)

keyBack <- ReplyKeyboardMarkup(
  keyboard = list(
    list(KeyboardButton("/start \xF0\x9F\x94\x83"))
  )
)

# > Custom keywords as Strings ----
# each lists represents a case for an answer, keywords are sorted by the last
# name of the keywords object, e.g. simone.csv for wordsSimone etc. 
# answers to this keywords are also sorted by the last name, e.g., simone.txt
# for cases of wordsSimone
# Sometimes emojis are included by Byte-definition
# Persons
wordsExam <- read.csv(
  "./keys/exam.csv",
  sep = "\n",
  header = TRUE
)[[1]]

wordsFail <- read.csv(
  "./keys/fail.csv",
  sep = "\n",
  header = TRUE
  )[[1]]

wordsGrade <- read.csv(
  "./keys/grade.csv",
  sep = "\n",
  header = TRUE
)[[1]]

wordsIll <- read.csv(
  "./keys/ill.csv",
  sep = "\n",
  header = TRUE
)[[1]]

wordsOffice <- read.csv(
  "./keys/Office.csv",
  sep = "\n",
  header = TRUE
)[[1]]

wordsPlace <- read.csv(
  "./keys/place.csv",
  sep = "\n",
  header = TRUE
)[[1]]

wordsPreexam <- read.csv(
  "./keys/preexam.csv",
  sep = "\n",
  header = TRUE
)[[1]]

wordsRegistration <- read.csv(
  "./keys/registration.csv",
  sep = "\n",
  header = TRUE
)[[1]]

# > Starting Updater ----
# usethis::edit_r_environ("project")
# this should be in .Renviron file bot_token() function
updater <- Updater(token = bot_token("RTelegramBot"))

# > Functions ----
start <- function(bot, update){
  bot$sendMessage(
    chat_id = update$message$chat_id,
    text = sprintf(
      paste0(
        readLines(
          "./texts/start.txt",
          encoding = "UTF-8"
        ),
        collapse = "\n"
      ),
      update$message$from$first_name
    ),
    reply_markup = keyStart
  )
}

case <- function(bot, update){
  text <- update$message$text
  
  if ((any(str_detect(text, regex(wordsExam, ignore_case = TRUE))))){
    text_caps <- paste0(
      readLines(
        "./texts/exam.txt",
        encoding = "UTF-8"
      ),
      collapse = "\n"
    )
      
    bot$sendMessage(
      chat_id = update$message$chat_id,
      text = text_caps,
      parse_mode = "Markdown",
      reply_markup = keyExams
    ) 
  } else if ((any(str_detect(text, regex(wordsOffice, ignore_case = TRUE))))) {
    text_caps <- paste0(
      readLines(
        "./texts/office.txt",
        encoding = "UTF-8"
      ),
      collapse = "\n"
    )
    
    bot$sendMessage(
      chat_id = update$message$chat_id,
      text = text_caps,
      parse_mode = "Markdown",
      reply_markup = keyBack
    ) 
  } else if  ((any(str_detect(text, regex(wordsRegistration, ignore_case =TRUE))))) {
    text_caps <- paste0(
      readLines(
        "./texts/registration.txt",
        encoding = "UTF-8"
      ),
      collapse = "\n"
    )
    
    bot$sendMessage(
      chat_id = update$message$chat_id,
      text = text_caps,
      parse_mode = "Markdown",
      reply_markup = keyBack
    )
  } else if  ((any(str_detect(text, regex(wordsIll, ignore_case =TRUE))))) {
    text_caps <- paste0(
      readLines(
        "./texts/ill.txt",
        encoding = "UTF-8"
      ),
      collapse = "\n"
    )
    
    bot$sendMessage(
      chat_id = update$message$chat_id,
      text = text_caps,
      parse_mode = "Markdown",
      reply_markup = keyBack
    )
  } else if  ((any(str_detect(text, regex(wordsPlace, ignore_case =TRUE))))) {
    text_caps <- paste0(
      readLines(
        "./texts/place.txt",
        encoding = "UTF-8"
      ),
      collapse = "\n"
    )
    
    bot$sendMessage(
      chat_id = update$message$chat_id,
      text = text_caps,
      parse_mode = "Markdown",
      reply_markup = keyBack
    )
  } else if  ((any(str_detect(text, regex(wordsFail, ignore_case =TRUE))))) {
    text_caps <- paste0(
      readLines(
        "./texts/fail.txt",
        encoding = "UTF-8"
      ),
      collapse = "\n"
    )
    
    bot$sendMessage(
      chat_id = update$message$chat_id,
      text = text_caps,
      parse_mode = "Markdown",
      reply_markup = keyBack
    )
  } else if  (any(str_detect(text, regex(wordsPreexam, ignore_case =TRUE)))) {
    text_caps <- paste0(
      readLines(
        "./texts/preexam.txt",
        encoding = "UTF-8"
      ),
      collapse = "\n"
    )
    
    bot$sendMessage(
      chat_id = update$message$chat_id,
      text = text_caps,
      parse_mode = "Markdown",
      reply_markup = keyBack
    )
  } else if  (any(str_detect(text, regex(wordsGrade, ignore_case =TRUE)))) {
    text_caps <- paste0(
      readLines(
        "./texts/grade.txt",
        encoding = "UTF-8"
      ),
      collapse = "\n"
    )
    
    bot$sendMessage(
      chat_id = update$message$chat_id,
      text = text_caps,
      parse_mode = "Markdown",
      reply_markup = keyBack
    )
  } else {
    text_caps = paste0(
      readLines(
        "./texts/sorry.txt",
        encoding = "UTF-8"
      ),
      collapse = "\n"
    )
    
    bot$sendMessage(
      chat_id = update$message$chat_id, 
      text = text_caps,
      reply_markup = keyBack
    )
  }
}

unknown <- function(bot, update){
  bot$sendMessage(
    chat_id = update$message$chat_id,
    text = paste0(
      readLines(
        "./texts/unknown.txt",
        encoding = "UTF-8",
        warn = FALSE
      ),
      collapse = "\n"
    ),
    reply_markup = keyBack
  )
}

# > Loading Functions ----
start_handler <- CommandHandler("start", start)
updater <- updater + start_handler
updater <- updater + MessageHandler(case, MessageFilters$text)
updater <- updater + MessageHandler(unknown, MessageFilters$command)

# > Start Bot ----
updater$start_polling()

