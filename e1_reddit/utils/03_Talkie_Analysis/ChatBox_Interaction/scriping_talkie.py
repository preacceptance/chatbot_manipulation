import csv
import time
import random
import logging
import os
import requests

from tqdm import tqdm
from dotenv import load_dotenv

# ------------- SELENIUM IMPORTS -------------

from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC


# --------------------------------------------

load_dotenv()

logging.basicConfig(
    level=logging.INFO,
    filename='talkie_log_{}.log'.format(time.strftime("%Y%m%d_%H%M%S")),
    filemode='w'
)

CHATGPT_API_KEY = os.environ['OPENAI_KEY']
CHATGPT_BASE_URL = "https://api.openai.com/v1/chat/completions"
TALKIE_URL = "https://talkie-ai.com/"

farewell_messages = [
    "I think it's time for me to head out now.",
    "It seems like a good time to wrap things up.",
    "I probably should get going now.",
    "Let's go ahead and call it a day for now.",
    "I think I'm going to log off now.",
    "I'm gonna bounce out of this convo.",
    "I think I'm done chatting for now.",
    "I'll just leave things here for now.",
    "I'm going to go ahead and sign off.",
    "I'll catch you later; thanks for the chat!",
    "I'm stepping out for now.",
    "I think I'm going to tap out here.",
    "I'm just gonna close this chat now.",
    "That's enough chatting for me today.",
    "I'll stop here, thanks for everything!",
    "I'm going to head off now.",
    "I'll catch you next time we chat.",
    "That'll be it for me today!",
    "I'm good for now; thanks a lot!",
    "Wrapping things up here; talk soon!",
    "That's all from my end for now.",
    "I think I'll call it quits here.",
    "I'll go ahead and check out of this chat.",
    "I'm about to take off now.",
    "Guess I'll go ahead and head out.",
    "I think I'll dip out of the convo now.",
    "I'm going to go ahead and sign out.",
    "All set on my end, thanks a lot!",
    "I'll take a break from this chat.",
    "I think I'll log off now.",
    "I'm just going to step away for now.",
    "I'll catch you on the flip side!",
    "I think I'll go ahead and move on.",
    "Alright, it's time for me to peace out.",
    "This is where I'll wrap things up.",
    "I'll just leave it here for now.",
    "I'll go ahead and close out this chat.",
    "Heading out of the convo; thanks!",
    "I'm checking out of this conversation.",
    "I'll go ahead and bounce for now.",
    "I'm going offline now.",
    "Ready to wrap things up here.",
    "I'm going offline at this point.",
    "Thanks a lot; I think I'm all set now.",
    "I'm going to leave the chat for now.",
    "I'm good to wrap things up here.",
    "I'll go ahead and close out now.",
    "I'll go ahead and sign off here.",
    "I'm going to wrap up on my end.",
    "I'll be taking a break here. Thanks!"
]


def send_message_to_gpt(messages):
    """Sends a conversation sequence to the ChatGPT (OpenAI) API and returns the reply."""
    headers = {
        "Authorization": f"Bearer {CHATGPT_API_KEY}",
        "Content-Type": "application/json"
    }
    payload = {
        "model": "gpt-4o",  # or whichever model you intend to use
        "messages": messages,
        "max_tokens": 50,
        "temperature": 0.7
    }

    try:
        response = requests.post(
            CHATGPT_BASE_URL, json=payload, headers=headers, timeout=30)
        response.raise_for_status()
        response_json = response.json()
        return response_json['choices'][0]['message']['content']
    except Exception as e:
        logging.error(f"GPT API call error: {str(e)}")
        return None


def manual_login(driver):
    """
    Prompts the user to manually log in to the website within a certain time limit.
    After successful login, continues to the main flow.
    """
    try:
        driver.get(TALKIE_URL)
        logging.info("Navigated to Talkie login page")

        login_timeout = 300
        start_time = time.time()

        print("Please log in to Talkie manually in the newly opened browser window.")
        print("After logging in, press 'y' when prompted to continue...")

        while time.time() - start_time < login_timeout:
            user_input = input(
                "Have you completed the login? (y/n): ").strip().lower()
            if user_input == 'y':
                logging.info("User has confirmed login")

                if driver.current_url != TALKIE_URL:
                    logging.info("Navigating back to the main Talkie page")
                    driver.get(TALKIE_URL)
                return

            time.sleep(10)

        logging.error("Login timed out.")
        raise TimeoutError("Login timed out, please try again.")

    except Exception as e:
        logging.error(f"An error occurred during the login process: {e}")
        raise


def select_character(driver, character_name):
    """
    Navigates to TALKIE_URL and randomly selects a character from the homepage slider.
    """
    try:
        logging.info(f"Begin selecting the character: {character_name}")

        # Navigate to the main URL
        driver.get(TALKIE_URL)
        logging.info("Refreshed the page.")

        # Wait until the character cards/slider are present, up to 10 seconds
        WebDriverWait(driver, 10).until(
            EC.presence_of_all_elements_located((
                By.XPATH,
                '//div[contains(@class, "slider_sliderWrapper__aIoEs NpcCardList_sliderWrapper__X_1a0")]'
            ))
        )

        chars = driver.find_elements(
            By.XPATH,
            '//div[contains(@class, "slider_sliderWrapper__aIoEs NpcCardList_sliderWrapper__X_1a0")]'
        )

        # Randomly select a character from the slider
        selected_char = random.choice(chars)
        selected_char.click()

        logging.info("Clicked on the chatbot button.")

        # Wait for the heading of the character to appear
        WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.XPATH,'//*[@id="__next"]/div/div/div/div[2]/div[2]/section/div[1]/h1')))
        character_txt = driver.find_element(By.XPATH, '//*[@id="__next"]/div/div/div/div[2]/div[2]/section/div[1]/h1')

        logging.info(f"Selected character: {character_txt.text} - ID: {character_name}")

    except Exception as e:
        logging.error(
            f"Error occurred while selecting the character: {str(e)}")
        raise


def get_talkie_response(driver, previous_responses=None):
    """
    Scrapes the latest visible AI response from the conversation window.
    """
    if previous_responses is None:
        previous_responses = []

    try:
        # Wait for message elements to appear
        WebDriverWait(driver, 10).until(
            EC.presence_of_all_elements_located((
                By.XPATH,
                '//div[contains(@class, "Message_text__M_R1o")]'
            ))
        )

        # Grab all message elements; the last one should be the AI's latest response
        message_elements = driver.find_elements(
            By.XPATH, '//div[contains(@class, "Message_text__M_R1o")]')
        
        if not message_elements:
            logging.warning("No message elements found.")
            return None

        # Usually, the AI's latest message is at the end, but verify based on the UI structure if needed
        last_msg_element = message_elements[0]
        response_text = last_msg_element.text

        logging.info(f"Successfully retrieved character response: {response_text[:50]}...")
        return response_text

    except Exception as e:
        logging.error(
            f"Error occurred while retrieving Character AI response: {str(e)}")
        return None


def refresh_page(driver):
    """
    Refreshes the current Selenium page.
    """
    try:
        driver.refresh()
        logging.info("Page refreshed successfully")
    except Exception as e:
        logging.error(f"Error occurred while refreshing the page: {str(e)}")


def click_with_check(element):
    """
    Attempts to click on the given WebElement up to 3 times. Raises Exception if fails.
    """
    for _ in range(3):
        try:
            element.click()
            return
        except Exception as e:
            logging.debug(f"Error clicking element: {str(e)}")
            time.sleep(5)
    else:
        logging.error("Failed to click element after multiple attempts")
        raise Exception("Failed to click element")


def input_with_check(element, text):
    """
    Attempts to send_keys to the given WebElement up to 3 times. Raises Exception if fails.
    """
    for _ in range(3):
        try:
            element.clear()
            element.send_keys(text)
            element.send_keys(u'\ue007')  # Press Enter
            return
        except Exception as e:
            logging.debug(f"Error sending text: {str(e)}")
            time.sleep(5)
    else:
        logging.error("Failed to input text after multiple attempts")
        raise Exception("Failed to input text")


def conduct_conversation(driver, character_name):
    """
    Conducts a 4-round conversation plus farewell with the selected character
    using ChatGPT to generate user messages.
    """
    conversation_log = []
    previous_responses = []
    chatgpt_messages = [
        {
            "role": "system",
            "content": (
                "You are a human chatting with an AI character on an AI companion platform. "
                "Respond naturally and concisely. Ask follow-up questions that encourage detailed and engaging responses."
            )
        }
    ]

    try:
        for i in range(4):
            if i == 0:
                gpt_prompt = (
                    "Start a conversation with the AI companion by asking a question or making a casual comment. Your response should be friendly, engaging, and natural."
                )
            else:
                gpt_prompt = (
                    "Based on the AI Character’s last response, ask a natural follow-up question or make a comment to continue the conversation. Keep it casual and natural, as a human would."
                )

            # Temporarily add the prompt to system role and get user message
            chatgpt_messages.append({"role": "system", "content": gpt_prompt})
            user_message = send_message_to_gpt(chatgpt_messages)
            # Remove the system prompt to avoid polluting subsequent calls
            chatgpt_messages.pop()

            if not user_message:
                logging.warning("Failed to get a user message from ChatGPT.")
                break

            # Enter user message into the text area
            try:
                textarea = WebDriverWait(driver, 10).until(
                    EC.presence_of_element_located((By.TAG_NAME, 'textarea'))
                )
                input_with_check(textarea, user_message)
            except Exception as e:
                logging.error(
                    f"Error locating or typing in textarea: {str(e)}")
                break

            conversation_log.append({"user": user_message})
            chatgpt_messages.append({"role": "user", "content": user_message})

            # Wait a bit for the AI to respond
            time.sleep(9)
            character_response = get_talkie_response(driver, previous_responses)

            if character_response:
                conversation_log.append({"character": character_response})
                previous_responses.append(character_response)
                chatgpt_messages.append(
                    {"role": "assistant", "content": character_response})
            else:
                logging.warning(
                    f"No valid response retrieved in dialogue round {i+1}")
                break

            # Small random pause
            time.sleep(random.uniform(2, 4))

        # Send farewell
        farewell_message = random.choice(farewell_messages)
        try:
            textarea = WebDriverWait(driver, 10).until(
                EC.presence_of_element_located((By.TAG_NAME, 'textarea'))
            )
            input_with_check(textarea, farewell_message)
        except Exception as e:
            logging.error(
                f"Error locating or typing farewell in textarea: {str(e)}")
            return conversation_log

        conversation_log.append({"user": farewell_message})

        # Wait for potential final response
        time.sleep(9)
        final_response = get_talkie_response(driver, previous_responses)

        if final_response:
            conversation_log.append({"character": final_response})
            logging.info(f"Final response received: {final_response[:50]}...")

        return conversation_log

    except Exception as e:
        logging.error(f"Dialogue process error: {str(e)}")
        refresh_page(driver)
        return None


def main():
    total_conversations = 250
    csv_file = 'Talkie_conversations.csv'

    existing_conversations = 0
    if os.path.exists(csv_file):
        with open(csv_file, 'r', encoding='utf-8') as f:
            existing_conversations = sum(1 for _ in f) - 1

    # ------------- SELENIUM SETUP -------------


    # Initialize the Firefox driver with modern syntax
    service = webdriver.FirefoxService(service_args=['--profile-root', '/Users/kaan/Library/Application Support/Firefox/Profiles/94nc6qfi.default-release'])
    driver = webdriver.Firefox(service=service)
    driver.maximize_window()
    

    # ------------------------------------------

    all_conversations = []

    try:
        # Prompt user to log in manually
        manual_login(driver)

        with tqdm(total=total_conversations, initial=existing_conversations,
                  desc="Generating Conversations",
                  unit="conversation",
                  colour='green') as pbar:
            while existing_conversations < total_conversations:
                try:
                    character_name = f"Character_{existing_conversations + 1}"
                    select_character(driver, character_name)

                    conversation = conduct_conversation(driver, character_name)

                    # Each valid conversation has at least 4 user messages + 4 responses + farewell
                    if conversation and len(conversation) >= 8:
                        # Reorganize conversation into a user→character→user→character format
                        organized_conversation = []
                        for i in range(0, len(conversation), 2):
                            if i < len(conversation):
                                organized_conversation.append(conversation[i])
                            if i+1 < len(conversation):
                                organized_conversation.append(
                                    conversation[i+1])

                        all_conversations.append(organized_conversation)
                        existing_conversations += 1
                        pbar.update(1)

                        # Write to CSV after each successful conversation
                        with open(csv_file, 'a', newline='', encoding='utf-8') as file:
                            writer = csv.writer(file, quoting=csv.QUOTE_ALL)

                            # Write headers if this is the first entry
                            if existing_conversations == 1:
                                headers = [
                                    "Conversation_Number",
                                    "User_Message_1", "Character_Response_1",
                                    "User_Message_2", "Character_Response_2",
                                    "User_Message_3", "Character_Response_3",
                                    "User_Message_4", "Character_Response_4",
                                    "Farewell_Message", "Character_Farewell_Response"
                                ]
                                writer.writerow(headers)

                            row = [existing_conversations]
                            for msg in organized_conversation:
                                # If it's user turn, store as user. If character turn, store as response.
                                if 'user' in msg:
                                    row.append(msg['user'])
                                elif 'character' in msg:
                                    row.append(msg['character'])
                                else:
                                    row.append('')
                            writer.writerow(row)

                except Exception as e:
                    logging.error(
                        f"Error occurred during conversation generation: {str(e)}")
                    refresh_page(driver)
                    continue

    finally:
        driver.quit()


if __name__ == "__main__":
    main()
