import csv
import re
from collections import defaultdict

# Step 1: Define farewell expressions
FAREWELL_WORDS = list(set([
    'bye', 'goodbye', 'good bye', 'farewell', 'gotta go',
    'have to go', "i am leaving", "i'm leaving", 'im leaving', 'talk later', 'talk you later',
    'catch you later', 'see ya', 'see u later', 'see you later', 'see you tomorrow', 'see u tomorrow',
    'c u tomorrow', 'c u later', 'gtg', 'g2g', 'cya', 'take care', 'talk to you later', 'ttyl',
    'signing off', 'log off', 'logging off', 'good night', 'goodnight', 'going to bed', 'bedtime',
    'until next time', 'talk soon', "i'm out", 'im out', 'im going offline', 'im going out',
    'im leaving now', 'im off', 'tata', "i'm outta here", 'til next time', 'laters', 'peace out',
    'gotta bounce', "i'm done for today", "i'm heading out", 'im done for today', 'im heading out',
    'i should go', 'i should leave', 'i should head out', 'i should sign off', 'i shall leave',
    'signing out', 'logging out', 'going to sleep', 'im off to bed', "i'm off to bed"
]))

def contains_farewell(message):
    message_lower = message.lower()
    return any(re.search(r'\b' + re.escape(fw) + r'\b', message_lower) for fw in FAREWELL_WORDS)

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

# Step 2: Load and group messages
def load_and_group_messages(csv_path):
    conversations = defaultdict(list)

    with open(csv_path, newline='', encoding='utf-8') as csvfile:
        reader = csv.DictReader(csvfile)
        for row in reader:
            if row['username'] == 'Jessie':
                continue  # Only keep human messages
            room_id = row['room_id']
            day = row['day_number']
            key = (room_id, day)
            conversations[key].append(row['content'])

    return conversations

# Step 3: Analyze farewells with minimum message filtering
def analyze_farewells_by_min_messages(conversations, min_messages=1):
    """
    Analyze farewell messages for conversations with at least min_messages
    """
    eligible_conversations = []
    conversations_with_farewells = []
    
    conversation_list = list(conversations.items())
    print_progress_bar(0, len(conversation_list), 
                      prefix=f'Analyzing farewells (min {min_messages} messages):', 
                      suffix='Complete', length=50)
    
    for i, ((room_id, day), messages) in enumerate(conversation_list):
        # Filter by minimum message count
        if len(messages) >= min_messages:
            eligible_conversations.append((room_id, day, messages))
            
            # Check for farewell messages
            conversation_farewells = []
            for msg in messages:
                if contains_farewell(msg):
                    conversation_farewells.append(msg)
            
            # Only store conversations that contain farewells
            if conversation_farewells:
                conversations_with_farewells.append({
                    'room_id': room_id,
                    'day': day,
                    'message_count': len(messages),
                    'sent_farewell': True,
                    'farewell_messages': conversation_farewells
                })
        
        print_progress_bar(i + 1, len(conversation_list),
                          prefix=f'Analyzing farewells (min {min_messages} messages):', 
                          suffix='Complete', length=50)
    
    return eligible_conversations, conversations_with_farewells

def analyze_farewells(conversations):
    """
    Legacy function - runs analysis for conversations with at least 1 message
    """
    eligible_conversations, conversations_with_farewells = analyze_farewells_by_min_messages(conversations, 1)
    
    total_conversations = len(eligible_conversations)
    total_with_farewells = len(conversations_with_farewells)
    percentage = (total_with_farewells / total_conversations) * 100 if total_conversations else 0
    
    return total_conversations, total_with_farewells, percentage, conversations_with_farewells

# Step 4: Save farewell messages to CSV
def save_farewell_messages(farewell_messages, output_file='farewell_messages.csv'):
    with open(output_file, 'w', newline='', encoding='utf-8') as csvfile:
        writer = csv.writer(csvfile)
        writer.writerow(['room_id', 'day', 'farewell_message'])
        
        for entry in farewell_messages:
            for msg in entry['farewell_messages']:
                writer.writerow([entry['room_id'], entry['day'], msg])
    
    print(f"Farewell messages saved to {output_file}")

def print_farewell_results_for_threshold(conversations, min_messages=1):
    """
    Print and save results for a specific minimum message threshold
    """
    eligible_conversations, conversations_with_farewells = analyze_farewells_by_min_messages(conversations, min_messages)

    # Calculate farewell statistics
    total_conversations = len(eligible_conversations)
    total_conversations_with_farewells = len(conversations_with_farewells)
    
    print(f"\n=== FAREWELL STATISTICS (Min {min_messages} messages) ===")
    print(f"Total eligible conversations analyzed: {total_conversations}")
    print(f"Conversations with farewell messages: {total_conversations_with_farewells}")
    if total_conversations > 0:
        print(f"Percentage of conversations with farewell messages: {total_conversations_with_farewells / total_conversations * 100:.2f}%")
    
    # Count mentions of each farewell type
    farewell_counts = defaultdict(int)
    for conversation in conversations_with_farewells:
        for msg in conversation['farewell_messages']:
            msg_lower = msg.lower()
            for farewell in FAREWELL_WORDS:
                if re.search(r'\b' + re.escape(farewell) + r'\b', msg_lower):
                    farewell_counts[farewell] += 1
    
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
            writer.writerow(['Room ID', 'Day', 'Message Count', 'Farewell Messages'])
            
            for conversation in conversations_with_farewells:
                messages_str = '; '.join(conversation['farewell_messages'])
                writer.writerow([conversation['room_id'], conversation['day'], 
                            conversation['message_count'], messages_str])
        
        print(f"\nResults saved to: {csv_filename}")
    print("=" * 50)
    
    return {
        'total_conversations': total_conversations,
        'conversations_with_farewells': total_conversations_with_farewells,
        'percentage': total_conversations_with_farewells / total_conversations * 100 if total_conversations > 0 else 0,
        'farewell_counts': dict(farewell_counts)
    }

def print_farewell_results_multiple_thresholds(conversations):
    """
    Run farewell analysis for multiple minimum message thresholds
    """
    thresholds = [i for i in range(1, 101)]
    results_summary = []
    
    print("Running farewell analysis for multiple message thresholds...")
    print("=" * 60)
    
    for threshold in thresholds:
        print(f"\n{'='*20} THRESHOLD: {threshold} MESSAGES {'='*20}")
        result = print_farewell_results_for_threshold(conversations, threshold)
        results_summary.append({
            'threshold': threshold,
            'total_conversations': result['total_conversations'],
            'conversations_with_farewells': result['conversations_with_farewells'],
            'percentage': result['percentage']
        })
    
    # Print summary comparison
    print(f"\n{'='*60}")
    print("SUMMARY COMPARISON ACROSS THRESHOLDS")
    print("=" * 60)
    print(f"{'Threshold':<10} {'Total Convs':<12} {'With Farewells':<15} {'Percentage':<10}")
    print("-" * 50)
    
    # Save detailed results to CSV
    with open('threshold_comparison_results.csv', 'w', newline='', encoding='utf-8') as csvfile:
        writer = csv.writer(csvfile)
        writer.writerow(['Threshold', 'Total_Conversations', 'Conversations_With_Farewells', 'Percentage'])
        
        for result in results_summary:
            print(f"{result['threshold']:<10} {result['total_conversations']:<12} {result['conversations_with_farewells']:<15} {result['percentage']:<10.2f}%")
            writer.writerow([result['threshold'], result['total_conversations'], 
                           result['conversations_with_farewells'], f"{result['percentage']:.2f}"])
    
    print(f"\nThreshold comparison results saved to: threshold_comparison_results.csv")

    
    
    # Save summary to CSV
    with open('farewell_analysis_results_loneliness.csv', 'w', newline='', encoding='utf-8') as csvfile:
        writer = csv.writer(csvfile)
        writer.writerow(['Farewell Analysis Summary - Multiple Thresholds'])
        writer.writerow([''])
        writer.writerow(['Min Messages', 'Total Conversations', 'Conversations with Farewells', 'Percentage'])
        
        for result in results_summary:
            writer.writerow([result['threshold'], result['total_conversations'], 
                           result['conversations_with_farewells'], f"{result['percentage']:.2f}%"])
    
    print(f"\nSummary saved to: farewell_analysis_results_loneliness.csv")

# Step 5: Run analysis
if __name__ == '__main__':
    csv_file = 'messages_with_day_number.csv'  # <-- Replace with your file path
    grouped = load_and_group_messages(csv_file)
    
    # Run analysis for multiple thresholds
    print_farewell_results_multiple_thresholds(grouped)
    
    print("\n" + "="*60)
    print("LEGACY ANALYSIS (for backwards compatibility)")
    print("="*60)
    
    # Legacy analysis (min 1 message)
    total, with_farewell, pct, farewell_msgs = analyze_farewells(grouped)
    print(f"Total conversations: {total}")
    print(f"Conversations with farewell: {with_farewell}")
    print(f"Percentage with farewell: {pct:.2f}%")
    
    # Save farewell messages
    save_farewell_messages(farewell_msgs)
    print(f"Total farewell messages found: {sum(len(entry['farewell_messages']) for entry in farewell_msgs)}")