async function form_submit() {
    const when = document.getElementById("when").value;
    const how_many = parseInt(document.getElementById("how-many").value);
    const where_to = document.getElementById("where-to").value;
    const email = document.getElementById("email").value;
    let request_payload = {
        amount: how_many,
        delivery_time: when,
        email: email,
        address: where_to
    };
    const response = await fetch("/icanhascookie", {
        method: "POST",
        headers: {
            "Content-Type": "application/json"
        },
        body: JSON.stringify(request_payload)
    });
    if (response.ok) {
        console.log("Order posted successfully!");
    } else {
        text = await response.text();
        console.log(`Error: got ${response.status}: ${text}`);
    }
};

document.getElementById("submit").onclick = form_submit;
