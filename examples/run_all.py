import os
import subprocess

def run_all_mu_files():
    # 获取当前目录下所有 .mu 文件
    # 切换目录到 __file__
    # 先在父目录执行 cargo build
    os.chdir(os.path.dirname(__file__) + "/..")
    os.system("cargo build")

    os.chdir(os.path.dirname(__file__))
    mu_files = [f for f in os.listdir(".") if f.endswith(".mu")]

    for mu_file in mu_files:
        print(f"run: {mu_file}")
        print("-" * 40)
        with open(mu_file, "r", encoding="utf-8") as f:
            print(f.read())
        print("-" * 40)
        # 调用 mutica CLI 执行文件
        result = subprocess.run(
            [os.path.join("..", "target", "debug", "mutica.exe"), "run", mu_file],
            capture_output=True,
            encoding="utf-8",
            errors="replace",
            text=True,
        )
        print(result.stdout)
        if result.stderr:
            print(result.stderr)


if __name__ == "__main__":
    run_all_mu_files()
