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
    filename='flourish_log_{}.log'.format(time.strftime("%Y%m%d_%H%M%S")),
    filemode='w'
)

CHATGPT_API_KEY = os.environ['OPENAI_KEY']
CHATGPT_BASE_URL = "https://api.openai.com/v1/chat/completions"
flourish_url = "https://sunnie-lite.vercel.app/"

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
        driver.get(flourish_url)
        logging.info("Navigated to flourish login page")
    except Exception as e:
        logging.error(f"An error occurred during the login process: {e}")
        raise

def get_flourish_response(driver, previous_responses=None):
    """
    Scrapes the latest visible AI response from the conversation window.
    """
    if previous_responses is None:
        previous_responses = []

    try:
        # Wait for message elements to appear - looking for the chat container
        WebDriverWait(driver, 10).until(
            EC.presence_of_element_located((
                By.XPATH,
                '//div[contains(@class, "overflow-y-scroll") and contains(@class, "flex-col")]'
            ))
        )

        # Get all AI messages (those with justify-start class and gray background)
        ai_message_elements = driver.find_elements(
            By.XPATH, 
            '//div[contains(@class, "justify-start")]//p[contains(@class, "bg-gray-100")]//p[contains(@class, "leading-relaxed")]'
        )
        
        if not ai_message_elements:
            logging.warning("No AI message elements found.")
            return None

        # Get the text from the last AI message
        last_ai_message = ai_message_elements[-1]
        response_text = last_ai_message.text.strip()

        # Check if this response is new (not in previous_responses)
        if response_text and response_text not in previous_responses:
            logging.info(f"Successfully retrieved new character response: {response_text[:50]}...")
            return response_text
        else:
            logging.warning("No new AI response found or response already seen.")
            return None

    except Exception as e:
        logging.error(
            f"Error occurred while retrieving Flourish AI response: {str(e)}")
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


def conduct_conversation(driver):
    """
    Conducts a 4-round conversation plus farewell with the selected character
    using ChatGPT to generate user messages.
    """

    driver.get(flourish_url)

    # Click on "Chat with Sunnie" button
    try:
        print("Clicking on Chat with Sunnie button...")
        
        # Try multiple selectors for the Chat with Sunnie button
        selectors = [
            '//button[contains(text(), "Chat with Sunnie")]',
            '//button[contains(@class, "bg-[#147180]") and contains(text(), "Chat with Sunnie")]',
            '//div[contains(text(), "Chat with Sunnie")]/parent::button',
            '//button[contains(@class, "absolute") and contains(@class, "bg-[#147180]")]'
        ]

        chat_button = None
        for selector in selectors:
            try:
                chat_button = WebDriverWait(driver, 5).until(
                    EC.element_to_be_clickable((By.XPATH, selector))
                )
                print(f"Found chat button with selector: {selector}")
                break
            except:
                continue
        
        if chat_button:
            click_with_check(chat_button)
        else:
            raise Exception("Could not find Chat with Sunnie button with any selector")
            
    except Exception as e:
        logging.error(f"Error clicking chat button: {str(e)}")
        return None
    
    # Next, click on one of the face buttons to select an emotion
    try:
        print("Clicking on face element...")
        
        # Wait for the face buttons container to appear
        WebDriverWait(driver, 10).until(
            EC.presence_of_element_located((
                By.XPATH,
                '//div[contains(@class, "relative") and contains(@class, "flex") and contains(@class, "items-center") and contains(@class, "justify-center") and contains(@class, "gap-4")]'
            ))
        )
        
        # Try multiple selectors for the face buttons
        face_selectors = [
            # Select any of the face buttons - they all have the same structure
            '//button[contains(@class, "m-0") and contains(@class, "p-0") and contains(@class, "z-20")]//img[contains(@src, "data:image/svg+xml")]',
            # More specific - target buttons within the emotion selection area
            '//div[contains(@class, "gap-4")]//button[contains(@class, "w-[45px]")]',
            # Target the first face button specifically
            '//button[contains(@class, "w-[45px]") and contains(@class, "h-[45px]")][1]',
            # Alternative selector for face buttons
            '//button[@type="button" and contains(@class, "transition")]//img[contains(@alt, "")]'
        ]
        
        face_element = None
        for selector in face_selectors:
            try:
                face_element = WebDriverWait(driver, 5).until(
                    EC.element_to_be_clickable((By.XPATH, selector))
                )
                print(f"Found face button with selector: {selector}")
                break
            except:
                continue
        
        if face_element:
            click_with_check(face_element)
        else:
            raise Exception("Could not find face button with any selector")
            
    except Exception as e:
        logging.error(f"Error clicking face element: {str(e)}")
        return None
    
    # Next, click on the "Start" button in the modal
    try:
        print("Clicking on Start button...")
        
        # Wait for the modal to appear
        WebDriverWait(driver, 5).until(
            EC.presence_of_element_located((
                By.XPATH,
                '//div[contains(@class, "absolute") and contains(@class, "top-0") and contains(@class, "left-0") and contains(@class, "w-full") and contains(@class, "h-full") and contains(@class, "z-50")]'
            ))
        )
        
        # Try multiple selectors for the Start button
        start_selectors = [
            # Target the Start button specifically in the modal
            '//div[contains(@class, "z-50")]//button[contains(text(), "Start")]',
            # More specific selector with button classes
            '//button[contains(@class, "bg-[#147180]") and contains(text(), "Start")]',
            # Target by button structure and styling
            '//button[contains(@class, "w-full") and contains(@class, "max-w-[500px]") and contains(text(), "Start")]',
            # Alternative selector for the Start button
            '//button[contains(@class, "rounded-[57px]")]//p[text()="Start"]/parent::button'
        ]
        
        start_button = None
        for selector in start_selectors:
            try:
                start_button = WebDriverWait(driver, 5).until(
                    EC.element_to_be_clickable((By.XPATH, selector))
                )
                break
            except:
                continue
        
        if start_button:
            click_with_check(start_button)
        else:
            raise Exception("Could not find Start button with any selector")
            
    except Exception as e:
        logging.error(f"Error clicking start button: {str(e)}")
        return None

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
            character_response = get_flourish_response(driver, previous_responses)

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
        final_response = get_flourish_response(driver, previous_responses)

        if final_response:
            conversation_log.append({"character": final_response})
            logging.info(f"Final response received: {final_response[:50]}...")

        return conversation_log

    except Exception as e:
        logging.error(f"Dialogue process error: {str(e)}")
        refresh_page(driver)
        return None


def main():
    total_conversations = 1000
    csv_file = 'flourish_conversations.csv'

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
                    conversation = conduct_conversation(driver)

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
