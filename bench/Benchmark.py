from iron_mq import *

testClient = IronMQ(host="mq-aws-us-east-1.iron.io",
    project_id="54a241faaeffe60005000047",
    token="DG0HDEUlDvj-oyTdM9Ua9rTxw7w",
    protocol="https", port=443,
    api_version=1,
    config_file=None)

def doStuff():
    queues = testClient.queues
    queue = testClient.queue("default")
    queue.post("This is message number ")
    messageList = queue.get()
    messageId = messageList['messages'][0]['id']
    queue.delete(messageId)

if __name__ == "__main__":
    doStuff()

# if __name__ == "__main__":
#     import timeit
#     print timeit.timeit("doStuff()", setup="from __main__ import doStuff", number = 100)