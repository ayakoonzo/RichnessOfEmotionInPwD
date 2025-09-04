#!/usr/bin/env python3
# Read .docx files in a specified folder, run 8-emotion multi-label inference (Plutchik: joy, sadness,
# anticipation, surprise, anger, fear, disgust, trust), and write per-chunk scores + a summary CSV.

import os
import pandas as pd
import torch
from transformers import AutoTokenizer, AutoModelForSequenceClassification, pipeline
from docx import Document
import argparse
import sys
import csv

DIR = "tohoku-wrime-8emo-ml-best"
INPUT_FOLDER = "../../scripts/manual_edit_scripts/"  # Folder containing the .docx files to process
OUTPUT_FOLDER = "result_8emo_scores"    # Folder where results will be saved

# Process a docx file
def process_docx(file_path, limit=None):
    doc = Document(file_path)
    sentences = [p.text.replace("\u2028", "") for p in doc.paragraphs if p.text.strip()]  # Extract all non-empty paragraphs

    if limit is not None:
        sentences = sentences[:limit]  # Process only the specified number of lines

    # Prepare tokenizer
    tok = AutoTokenizer.from_pretrained(DIR)

    # Handle token length limit by splitting if necessary
    processed_sentences = []
    for i, sentence in enumerate(sentences, 1):
        tokens = tok.tokenize(sentence)
        if len(tokens) > 128:
            # If the number of tokens exceeds 128, split into chunks
            num_chunks = (len(tokens) + 127) // 128
            for chunk_idx in range(num_chunks):
                start_idx = chunk_idx * 128
                end_idx = start_idx + 128
                chunk_tokens = tokens[start_idx:end_idx]

                processed_sentence = tok.convert_tokens_to_string(chunk_tokens).replace(" ", "").strip()
                processed_sentences.append(processed_sentence)
                print(f"Line {i}.{chunk_idx + 1}: {processed_sentence} (Tokens: {len(chunk_tokens)})")
        else:
            processed_sentences.append(sentence)
            print(f"Line {i}: {sentence} (Tokens: {len(tokens)})")

    return processed_sentences

# Argument parser
def parse_arguments():
    parser = argparse.ArgumentParser(description="Process .docx files for 8-emotion (multi-label) analysis.")
    parser.add_argument("index", nargs="?", type=int, help="0 to list, 1..N to process only that file")
    parser.add_argument("--all", action="store_true", help="Process all files in the input directory")
    parser.add_argument("--limit", type=int, help="Number of lines to process in each document")
    return parser.parse_args()

# Main processing
if __name__ == "__main__":
    args = parse_arguments()

    if len(sys.argv) == 1:  # No arguments provided
        parser = argparse.ArgumentParser(description="Process .docx files for 8-emotion (multi-label) analysis.")
        parser.add_argument("index", nargs="?", type=int, help="0 to list, 1..N to process only that file")
        parser.add_argument("--all", action="store_true", help="Process all files in the input directory")
        parser.add_argument("--limit", type=int, help="Number of lines to process in each document")
        parser.print_help()
        sys.exit(0)

    docx_files = sorted([f for f in os.listdir(INPUT_FOLDER) if f.endswith(".docx")])

    if not docx_files:
        print(f"[error] no .docx files in {INPUT_FOLDER}")
        sys.exit(1)

    if args.all:
        print("[info] Processing all files in the input directory.")
        targets = docx_files
    elif args.index is not None:
        if args.index == 0:
            print("Available files:")
            for i, file_name in enumerate(docx_files, 1):
                print(f"{i:3}: {file_name}")
            sys.exit(0)
        elif 1 <= args.index <= len(docx_files):
            targets = [docx_files[args.index - 1]]
        else:
            print(f"[error] index out of range: 1..{len(docx_files)}")
            sys.exit(1)
    else:
        print("[error] No valid arguments provided. Use --all or specify an index.")
        sys.exit(1)

    # Load model and tokenizer
    tok = AutoTokenizer.from_pretrained(DIR)
    mdl = AutoModelForSequenceClassification.from_pretrained(DIR)
    device = "mps" if torch.backends.mps.is_available() else "cpu"

    clf = pipeline("text-classification", model=mdl, tokenizer=tok,
                   device=device, top_k=None, batch_size=64)

    EMOS = ["joy", "sadness", "anticipation", "surprise", "anger", "fear", "disgust", "trust"]

    # Process target files
    for file_name in targets:
        file_path = os.path.join(INPUT_FOLDER, file_name)
        print(f"Processing: {file_path}")
        processed_sentences = process_docx(file_path, args.limit)

        # Prepare output CSV
        output_csv_path = os.path.join(OUTPUT_FOLDER, f"{os.path.splitext(file_name)[0]}_8emo.csv")
        os.makedirs(OUTPUT_FOLDER, exist_ok=True)

        all_results = []  # Store all results

        with open(output_csv_path, mode="w", newline="", encoding="utf-8") as csvfile:
            csvwriter = csv.writer(csvfile)
            # Write header row
            csvwriter.writerow(["row_type", "file", "line", "token_len"] + [f"score_{emo}" for emo in EMOS] + ["text"])

            # Run emotion analysis (8 labels, multi-label)
            print("\nEmotion Analysis Results (8 labels):")
            for i, sentence in enumerate(processed_sentences, 1):
                result = clf(sentence)[0]  # Take the first element
                scores = {emo: next((x['score'] for x in result if x['label'] == emo), 0) for emo in EMOS}
                token_len = len(tok.tokenize(sentence))

                # Add result to list
                all_results.append({"token_len": token_len, **scores})

                # Write to CSV
                csvwriter.writerow(["chunk", file_name, f"line {i}", token_len] + [scores[emo] for emo in EMOS])

                # Print to console
                print(f"line {i}: {sentence} \n\ttoken_len: {token_len}, scores: {scores}")

            # Compute and display summary
            total_lines = len(all_results)
            total_tokens = sum(r["token_len"] for r in all_results)

            # Calculate average score for each emotion
            avg_scores = {emo: sum(r[emo] for r in all_results) / total_lines for emo in EMOS}
            avg_token_len = total_tokens / total_lines

            # Write summary row to CSV
            csvwriter.writerow([
                "summary", file_name, total_lines, total_tokens
            ] + [avg_scores[emo] for emo in EMOS] + [f"Average Token Length: {avg_token_len:.2f}"])

            print("\nSummary:")
            print(f"Total lines processed: {total_lines}")
            print(f"Total Tokens: {total_tokens}")
            for emo in EMOS:
                print(f"Average {emo.capitalize()}: {avg_scores[emo]:.4f}")
            print(f"Average Token Length: {avg_token_len:.2f}")
