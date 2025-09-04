#!/usr/bin/env python3
# pip install protobuf
# pip install "fugashi[unidic-lite]"

import torch, numpy as np, pandas as pd
from datasets import Dataset, DatasetDict, Sequence, Value
from transformers import BertJapaneseTokenizer, AutoModelForSequenceClassification, TrainingArguments, Trainer
from sklearn.metrics import f1_score

CSV = "wrime-ver2.tsv"            # TSV
DEVICE = "mps" if torch.backends.mps.is_available() else "cpu"
MODEL = "tohoku-nlp/bert-base-japanese-v3"
MAX_LEN = 128
SPLIT = "Train/Dev/Test"
TEXT_COL = "Sentence"

# FAST mode: set True for a quick smoke test (tiny subsets + few steps)
FAST = False   # <- change to True when you want a super quick run

# WRIME v2 average-reader columns for Plutchik's 8 emotions
EMOS = ["joy","sadness","anticipation","surprise","anger","fear","disgust","trust"]
LABEL_COLS = [
    "Avg. Readers_Joy",
    "Avg. Readers_Sadness",
    "Avg. Readers_Anticipation",
    "Avg. Readers_Surprise",
    "Avg. Readers_Anger",
    "Avg. Readers_Fear",
    "Avg. Readers_Disgust",
    "Avg. Readers_Trust",
]
THR = 0.5  # binarization threshold per emotion (simple baseline)

# 0) Load + normalize
df = pd.read_csv(CSV, sep="\t")
df.columns = df.columns.str.strip()

if SPLIT not in df.columns:
    raise RuntimeError(f"Missing split column '{SPLIT}'. Got: {list(df.columns)[:10]} ...")

# normalize split values
df[SPLIT] = df[SPLIT].astype(str).str.strip().str.lower().replace({
    "training":"train", "train ":"train",
    "validation":"dev", "val":"dev", "dev ":"dev",
    "test ":"test"
})

# basic column checks
missing = [c for c in [TEXT_COL, *LABEL_COLS] if c not in df.columns]
if missing:
    raise RuntimeError(f"Missing columns: {missing}")

# drop rows with missing text/labels
df = df.dropna(subset=[TEXT_COL, *LABEL_COLS]).copy()

# 8-dim multi-hot labels from Avg. Readers_* by thresholding
def row_to_multilabel(r):
    return [int(float(r[c]) >= THR) for c in LABEL_COLS]

df["labels"] = df.apply(row_to_multilabel, axis=1)

# split DataFrames
dfs = {
    "train": df[df[SPLIT] == "train"][[TEXT_COL,"labels"]].reset_index(drop=True),
    "validation": df[df[SPLIT] == "dev"][[TEXT_COL,"labels"]].reset_index(drop=True),
    "test": df[df[SPLIT] == "test"][[TEXT_COL,"labels"]].reset_index(drop=True),
}
print("DF counts:", {k: len(v) for k,v in dfs.items()})

# 1) HF Datasets
raw = DatasetDict({k: Dataset.from_pandas(v) for k,v in dfs.items()})
print("HF counts before tokenize:", {k: len(v) for k,v in raw.items()})

# 2) Tokenize (fixed 128 like your 3cls setup)
tok = BertJapaneseTokenizer.from_pretrained(MODEL)
def tokenize(b):
    return tok(b[TEXT_COL], padding="max_length", truncation=True, max_length=MAX_LEN)

tokenized = raw.map(tokenize, batched=True, remove_columns=[TEXT_COL])
tokenized = tokenized.cast_column("labels", Sequence(Value("float32")))  # <- make labels float
tokenized = tokenized.with_format("torch")


print("HF counts after tokenize:", {k: len(v) for k,v in tokenized.items()})
print("Sample keys:", tokenized["train"][0].keys())  # expect: input_ids, token_type_ids, attention_mask, labels

# [FAST] keep only tiny subsets so training finishes quickly
if FAST:
    def _tiny(d, n):
        n = min(n, len(d))
        return d.select(range(n))
    tokenized["train"] = _tiny(tokenized["train"], 64)
    tokenized["validation"] = _tiny(tokenized["validation"], 32)
    tokenized["test"] = _tiny(tokenized["test"], 32)
    print("[FAST] subset sizes:", {k: len(v) for k,v in tokenized.items()})

# 3) Model (multi-label: sigmoid)
id2label = {i: EMOS[i] for i in range(8)}
label2id = {v:k for k,v in id2label.items()}
mdl = AutoModelForSequenceClassification.from_pretrained(
    MODEL,
    num_labels=8,
    problem_type="multi_label_classification",  # <- important: sigmoid + BCE loss
    id2label=id2label, label2id=label2id
).to(DEVICE)

# 4) Metrics: sigmoid -> threshold(0.5) -> micro/macro F1
def metrics(eval_pred):
    logits, y = eval_pred                    # y: shape [B,8] with {0,1}
    probs = 1/(1+np.exp(-logits))            # sigmoid
    pred = (probs >= 0.5).astype(int)        # simple fixed threshold
    return {
        "f1_micro": f1_score(y, pred, average="micro", zero_division=0),
        "f1_macro": f1_score(y, pred, average="macro", zero_division=0),
    }

# 5) Training
if FAST:
    # ultra-short run: no eval/save, just a few updates
    args = TrainingArguments(
        output_dir="tohoku-wrime-8emo-ml",
        max_steps=5,
        per_device_train_batch_size=16,
        per_device_eval_batch_size=32,
        eval_strategy="no",
        save_strategy="no",
        logging_steps=1,
        report_to="none",
        seed=42,
    )
else:
    args = TrainingArguments(
        output_dir="tohoku-wrime-8emo-ml",
        learning_rate=2e-5,
        per_device_train_batch_size=16,
        per_device_eval_batch_size=32,
        num_train_epochs=3,
        weight_decay=0.01,
        eval_strategy="epoch",          # new key name in recent Transformers
        save_strategy="epoch",
        load_best_model_at_end=True,
        metric_for_best_model="f1_macro",
        report_to="none",
        seed=42,
    )

# 6) Trainer → train → test
trainer = Trainer(
    model=mdl,
    args=args,
    train_dataset=tokenized["train"],
    eval_dataset=tokenized["validation"],
    processing_class=tok,           # future-proof in place of 'tokenizer'
    compute_metrics=metrics
)

print("Device:", DEVICE)
print("Len(train_dataset) seen by Trainer:", len(trainer.train_dataset))
trainer.train()

# 6) Final test evaluation
# Final test evaluation (skip metrics if FAST without eval dataset)
if not FAST:
    print(trainer.evaluate(tokenized["test"]))
else:
    # quick forward-only sanity on test batch (optional)
    batch = tokenized["test"][:8]
    with torch.no_grad():
        logits = mdl(
            input_ids=batch["input_ids"].to(DEVICE),
            attention_mask=batch["attention_mask"].to(DEVICE),
            token_type_ids=batch.get("token_type_ids", None).to(DEVICE) if "token_type_ids" in batch else None
        ).logits
    print("[FAST] test logits shape:", tuple(logits.shape))
