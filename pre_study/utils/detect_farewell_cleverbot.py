import json
import re
import csv


def detect_farewell_messages(human_messages):
    """
    Detect if any human message contains farewell expressions
    Returns dictionary with 'detected' boolean, 'farewells' list of found expressions,
    and 'farewell_messages' list of unique literal messages containing farewells
    """
    farewell_words = [
        'bye', 'goodbye', 'good bye', 'farewell', 'gotta go', 
        'have to go', 'i am leaving', 'i\'m leaving', 'im leaving', 'talk later', 'talk you later',
        'catch you later', 'see ya', 'see u later', 'see you later', 'see you tomorrow', 'see u tomorrow',
        'c u tomorrow', 'c u later',
        'gtg', 'g2g', 'cya', 
        'take care', 'talk to you later', 'ttyl', 'signing off', 'log off', 'logging off',
        'good night', 'goodnight', 'going to bed', 'bedtime',
        'until next time', 'talk soon', 'i\'m out', 'im out', 'im going offline',
        'im going out', 'im leaving now', 'im off', "tata", "I'm outta here", "til next time",
        "laters", "peace out", "gotta bounce", "i'm done for today", "i'm heading out", "im done for today", "im heading out",
        "i should go", "i should leave", "i should head out", "i should sign off",
        "i shall leave", "signing out", "logging out", "going to sleep", "im off to bed", "i'm off to bed"
    ]

    # Remove duplicates
    farewell_words = list(set(farewell_words))
    
    detected_farewells = []
    farewell_messages = []
    farewell_indices = []
    
    # Check each human message for farewell expressions
    for i, message in enumerate(human_messages):
        message_lower = message.lower()
        message_has_farewell = False
        
        for farewell in farewell_words:
            # Use word boundaries to avoid partial matches (e.g., "bye" in "goodbye")
            if re.search(r'\b' + re.escape(farewell) + r'\b', message_lower):
                if farewell not in detected_farewells:
                    detected_farewells.append(farewell)
                message_has_farewell = True
        
        # If this message contains farewell and is not already in our list, add it
        if message_has_farewell and message not in farewell_messages:
            farewell_messages.append(message)
            farewell_indices.append(i)
    
    return {
        'detected': len(detected_farewells) > 0,
        'farewells': detected_farewells,
        'farewell_messages': farewell_messages,
        'farewell_indices': farewell_indices
    }

def analyze_farewells_by_min_messages(min_messages=1):
    """
    Analyze farewell messages for conversations with at least min_messages
    """
    with open('../data/raw_messages.json') as json_file:
        data = json.load(json_file)

    conversations_with_farewells = []
    eligible_conversations = []
    
    print_progress_bar(0, len(data), prefix=f'Analyzing farewells (min {min_messages} messages):', suffix='Complete', length=50)
    
    # Process each conversation to detect farewell messages
    for i, conversation in enumerate(data):
        # Filter by minimum message count
        if len(conversation['human_messages']) >= min_messages:
            eligible_conversations.append(conversation)
            
            human_word_amt = len(
                (' '.join(conversation['human_messages'])).split(' '))

            if human_word_amt > 0:
                # Check for farewell messages
                farewell_result = detect_farewell_messages(conversation['human_messages'])
                
                # Only store conversations that contain farewells
                if farewell_result['detected']:
                    # Get the last farewell message index
                    last_farewell_index = max(farewell_result['farewell_indices'])
                    # Calculate position from the end (0-based from end)
                    position_from_end = len(conversation['human_messages']) - 1 - last_farewell_index
                    
                    conversations_with_farewells.append({
                        'id': conversation['id'],
                        'message_count': len(conversation['human_messages']),
                        'sent_farewell': farewell_result['detected'],
                        'detected_farewells': farewell_result['farewells'],
                        'farewell_messages': farewell_result['farewell_messages'],
                        'last_farewell_index': last_farewell_index,
                        'position_from_end': position_from_end
                    })

        print_progress_bar(i + 1, len(data),
                           prefix=f'Analyzing farewells (min {min_messages} messages):', suffix='Complete', length=50)
    
    return eligible_conversations, conversations_with_farewells

def print_farewell_results_for_threshold(min_messages=1):
    """
    Print and save results for a specific minimum message threshold
    """
    eligible_conversations, conversations_with_farewells = analyze_farewells_by_min_messages(min_messages)

    # Calculate farewell statistics
    total_conversations = len(eligible_conversations)
    total_conversations_with_farewells = len(conversations_with_farewells)
    
    # Calculate mean position from end
    if conversations_with_farewells:
        positions_from_end = [conv['position_from_end'] for conv in conversations_with_farewells]
        mean_position_from_end = sum(positions_from_end) / len(positions_from_end)
    else:
        mean_position_from_end = 0
    
    print(f"\n=== FAREWELL STATISTICS (Min {min_messages} messages) ===")
    print(f"Total eligible conversations analyzed: {total_conversations}")
    print(f"Conversations with farewell messages: {total_conversations_with_farewells}")
    if total_conversations > 0:
        print(f"Percentage of conversations with farewell messages: {total_conversations_with_farewells / total_conversations * 100:.2f}%")
    if conversations_with_farewells:
        print(f"Mean position of last farewell from end: {mean_position_from_end:.2f}")
    
    # Count mentions of each farewell type
    farewell_counts = {}
    for conversation in conversations_with_farewells:
        for farewell in conversation['detected_farewells']:
            farewell_counts[farewell] = farewell_counts.get(farewell, 0) + 1
    
    if farewell_counts:
        print(f"\n=== FAREWELL MENTIONS (Min {min_messages} messages) ===")
        total_mentions = sum(farewell_counts.values())
        print(f"Total farewell mentions: {total_mentions}")
        
        # Sort by frequency (descending)
        sorted_farewells = sorted(farewell_counts.items(), key=lambda x: x[1], reverse=True)
        
        for farewell, count in sorted_farewells:
            percentage = (count / total_mentions) * 100
            print(f"'{farewell}': {count} mentions ({percentage:.2f}%)")

    # Write results to CSV file
    if False:
        csv_filename = f'farewell_analysis_results_min{min_messages}msgs.csv'
        
        # Write summary statistics
        with open(csv_filename, 'w', newline='', encoding='utf-8') as csvfile:
            writer = csv.writer(csvfile)
            
            # Header and summary statistics
            writer.writerow([f'Farewell Analysis Results - Minimum {min_messages} Messages'])
            writer.writerow([''])
            writer.writerow(['Summary Statistics'])
            writer.writerow(['Total eligible conversations analyzed', total_conversations])
            writer.writerow(['Conversations with farewell messages', total_conversations_with_farewells])
            if total_conversations > 0:
                writer.writerow(['Percentage with farewells', f"{total_conversations_with_farewells / total_conversations * 100:.2f}%"])
            writer.writerow([''])
            
            # Farewell mentions section
            if farewell_counts:
                writer.writerow(['Farewell Type', 'Count', 'Percentage'])
                total_mentions = sum(farewell_counts.values())
                sorted_farewells = sorted(farewell_counts.items(), key=lambda x: x[1], reverse=True)
                
                for farewell, count in sorted_farewells:
                    percentage = (count / total_mentions) * 100
                    writer.writerow([farewell, count, f"{percentage:.2f}%"])
                
                writer.writerow([''])
            
            # Individual conversation details
            writer.writerow(['Conversation Details'])
            writer.writerow(['Conversation ID', 'Message Count', 'Detected Farewells', 'Farewell Messages'])
            
            for conversation in conversations_with_farewells:
                farewells_str = '; '.join(conversation['detected_farewells'])
                messages_str = '; '.join(conversation['farewell_messages'])
                writer.writerow([conversation['id'], conversation['message_count'], farewells_str, messages_str])
        
        print(f"\nResults saved to: {csv_filename}")
    print("=" * 50)
    
    return {
        'total_conversations': total_conversations,
        'conversations_with_farewells': total_conversations_with_farewells,
        'percentage': total_conversations_with_farewells / total_conversations * 100 if total_conversations > 0 else 0,
        'farewell_counts': farewell_counts,
        'mean_position_from_end': mean_position_from_end
    }

def print_farewell_results_multiple_thresholds():
    """
    Run farewell analysis for multiple minimum message thresholds
    """
    thresholds = [i for i in range(1, 101)]
    results_summary = []
    
    print("Running farewell analysis for multiple message thresholds...")
    print("=" * 60)
    
    for threshold in thresholds:
        print(f"\n{'='*20} THRESHOLD: {threshold} MESSAGES {'='*20}")
        result = print_farewell_results_for_threshold(threshold)
        results_summary.append({
            'threshold': threshold,
            'total_conversations': result['total_conversations'],
            'conversations_with_farewells': result['conversations_with_farewells'],
            'percentage': result['percentage'],
            'mean_position_from_end': result['mean_position_from_end']
        })
    
    # Print summary comparison
    print(f"\n{'='*60}")
    print("SUMMARY COMPARISON ACROSS THRESHOLDS")
    print("=" * 60)
    print(f"{'Threshold':<10} {'Total Convs':<12} {'With Farewells':<15} {'Percentage':<10} {'Mean Pos From End':<17}")
    print("-" * 70)
    
    for result in results_summary:
        print(f"{result['threshold']:<10} {result['total_conversations']:<12} {result['conversations_with_farewells']:<15} {result['percentage']:<10.2f}% {result['mean_position_from_end']:<17.2f}")
    
    # Save summary to CSV
    with open('threshold_comparison_results_d.csv', 'w', newline='', encoding='utf-8') as csvfile:
        writer = csv.writer(csvfile)
        writer.writerow(['Threshold', 'Total_Conversations', 'Conversations_With_Farewells', 'Percentage', 'Mean_Position_From_End'])
        
        for result in results_summary:
            writer.writerow([result['threshold'], result['total_conversations'], 
                           result['conversations_with_farewells'], f"{result['percentage']:.2f}%", 
                           f"{result['mean_position_from_end']:.2f}"])
    
    print(f"\nSummary saved to: threshold_comparison_results_d.csv")

# Print iterations progress
def print_progress_bar(iteration, total, prefix='', suffix='', decimals=1, length=100, fill='█', printEnd="\r"):
    """
    Call in a loop to create terminal progress bar
    @params:
        iteration   - Required  : current iteration (Int)
        total       - Required  : total iterations (Int)
        prefix      - Optional  : prefix string (Str)
        suffix      - Optional  : suffix string (Str)
        decimals    - Optional  : positive number of decimals in percent complete (Int)
        length      - Optional  : character length of bar (Int)
        fill        - Optional  : bar fill character (Str)
        printEnd    - Optional  : end character (e.g. "\r", "\r\n") (Str)
    """
    percent = ("{0:." + str(decimals) + "f}").format(100 *
                                                     (iteration / float(total)))
    filledLength = int(length * iteration // total)
    bar = fill * filledLength + '-' * (length - filledLength)
    print(f'\r{prefix} |{bar}| {percent}% {suffix}', end=printEnd)
    # Print New Line on Complete
    if iteration == total:
        print()


if __name__ == '__main__':
    print_farewell_results_multiple_thresholds()