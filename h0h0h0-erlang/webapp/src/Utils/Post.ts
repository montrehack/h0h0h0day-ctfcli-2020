export async function post(url: string, body: any): Promise<any> {
  const response = await fetch(url, {
    method: "POST",
    headers: {
      "Content-Type": "application/json",
    },
    body: JSON.stringify(body),
  });
  if (response.status === 200) {
    return await response.json();
  }

  throw new Error("Unexpected error");
}
