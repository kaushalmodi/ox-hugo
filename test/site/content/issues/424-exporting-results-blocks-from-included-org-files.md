+++
title = "Issue 424 – Exporting Results blocks from Included Org files"
description = """
  Example of using `#+include` to export code block **and** results
  block from that included file.
  """
tags = ["issues", "include", "results"]
draft = false
+++

Note
: By default, Org will export only the code block. So if you
    want to export the `#+results` block as well, you need to add
    `:exports both` to the source block header.

<!--listend-->

```python
lm_adam = LinearModel()
lm_adam.to(device)
optimizer = Adam(lm_adam.parameters(), weight_decay=0.0001)

for epoch in range(epochs):
    running_loss = 0.0

    for i, (x, y) in enumerate(dummy_data):

        x_ = Variable(x, requires_grad=True)
        y_ = Variable(y)

        # Forward pass
        y_pred = lm_adam(x_)

        # Compute loss
        loss = criterion(y_pred, y_)

        # Zero gradients, backward pass, and update weights
        optimizer.zero_grad()
        loss.backward()
        optimizer.step()

        # Update the running loss
        running_loss += loss.item()

    print(f"Epoch: {epoch + 1:02}/{epochs} Loss: {running_loss:.5e}")
```

```text
Epoch: 01/20 Loss: 1.35205e+03
Epoch: 02/20 Loss: 1.33684e+03
Epoch: 03/20 Loss: 1.32229e+03
Epoch: 04/20 Loss: 1.30802e+03
Epoch: 05/20 Loss: 1.29396e+03
Epoch: 06/20 Loss: 1.28007e+03
Epoch: 07/20 Loss: 1.26634e+03
Epoch: 08/20 Loss: 1.25274e+03
Epoch: 09/20 Loss: 1.23927e+03
Epoch: 10/20 Loss: 1.22593e+03
Epoch: 11/20 Loss: 1.21272e+03
Epoch: 12/20 Loss: 1.19962e+03
Epoch: 13/20 Loss: 1.18664e+03
Epoch: 14/20 Loss: 1.17378e+03
Epoch: 15/20 Loss: 1.16103e+03
Epoch: 16/20 Loss: 1.14839e+03
Epoch: 17/20 Loss: 1.13587e+03
Epoch: 18/20 Loss: 1.12345e+03
Epoch: 19/20 Loss: 1.11114e+03
Epoch: 20/20 Loss: 1.09893e+03
```
