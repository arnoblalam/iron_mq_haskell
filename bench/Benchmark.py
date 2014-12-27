from iron_mq import *

testClient = IronMQ(host="mq-aws-us-east-1.iron.io",
    project_id="53f691bd45d4960005000082",
    token="_B6KOfA16D4AmW2NwSCw12mgVxk",
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