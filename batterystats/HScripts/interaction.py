from AndroidRunner.Device import Device
# noinspection PyUnusedLocal
def main(device, *args, **kwargs):
    print('=INTERACTION=')
    device.shell('input swipe 100 200 100 100 1000')
    time.sleep(4)
    print((device.id))
    print((device.current_activity()))
